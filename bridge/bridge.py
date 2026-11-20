# /// script
# requires-python = ">=3.10"
# dependencies = ["telethon>=1.36", "aiohttp>=3.9"]
# ///
"""Telegram user-account bridge for compos.

Logs in as the user's own Telegram account (Telethon, MTProto) and serves a
small JSON API on BRIDGE_HOST:BRIDGE_PORT. Every request needs the header
Authorization: Bearer $TELEGRAM_BRIDGE_TOKEN.

  GET  /status                         authorized?, me
  POST /login/code     {phone}         Telegram sends a login code
  POST /login/sign_in  {code}|{password}
  GET  /dialogs?limit=&archived=       chats, newest first
  GET  /messages?chat=&limit=&before=  one chat's messages, newest first
  GET  /search?q=&chat=&limit=         messages matching q, in one chat or all
  POST /send           {chat, text, reply_to?}
  POST /read           {chat}
  GET  /updates?after=&timeout=        long poll: new messages since seq AFTER
  GET  /download?chat=&id=             the media of one message, as bytes
"""
import asyncio
import os

from aiohttp import web
from telethon import TelegramClient, events, utils
from telethon.errors import SessionPasswordNeededError

API_ID = int(os.environ["TELEGRAM_API_ID"])
API_HASH = os.environ["TELEGRAM_API_HASH"]
TOKEN = os.environ["TELEGRAM_BRIDGE_TOKEN"]
HOST = os.environ.get("BRIDGE_HOST", "127.0.0.1")
PORT = int(os.environ.get("BRIDGE_PORT", "8792"))
HOME = os.path.expanduser(os.environ.get("BRIDGE_HOME", "~/telegram-bridge"))
FILES = os.path.join(HOME, "files")

client = TelegramClient(os.path.join(HOME, "user"), API_ID, API_HASH)
login = {}
log = []
seq = 0
changed = asyncio.Condition()


def iso(d):
    return d.isoformat() if d else None


def ts(d):
    return int(d.timestamp()) if d else None


def kind_of(entity):
    if entity is None:
        return "unknown"
    if getattr(entity, "bot", False):
        return "bot"
    if hasattr(entity, "first_name"):
        return "user"
    if getattr(entity, "broadcast", False):
        return "channel"
    return "group"


def media_of(m):
    if getattr(m, "action", None) is not None:
        name = type(m.action).__name__.replace("MessageAction", "")
        return {"type": "event", "name": name}
    if not m.media:
        return None
    if m.photo:
        return {"type": "photo"}
    if m.voice:
        return {"type": "voice", "duration": m.file.duration if m.file else None}
    if m.sticker:
        return {"type": "sticker", "emoji": getattr(m.file, "emoji", None)}
    if m.video:
        return {"type": "video"}
    if m.file:
        return {"type": "file", "name": m.file.name, "mime": m.file.mime_type, "size": m.file.size}
    return {"type": type(m.media).__name__}


def message_json(m, chat_name=None):
    sender = m.sender
    return {
        "id": m.id,
        "date": iso(m.date),
        "ts": ts(m.date),
        "out": bool(m.out),
        "sender_id": m.sender_id,
        "sender": utils.get_display_name(sender) if sender else (chat_name or ""),
        "text": m.message or "",
        "reply_to": m.reply_to_msg_id,
        "media": media_of(m),
    }


async def entity(chat):
    return await client.get_input_entity(int(chat))


def ok(data=None, **more):
    body = {"ok": True}
    if data is not None:
        body["result"] = data
    body.update(more)
    return web.json_response(body)


def fail(text, status=400):
    return web.json_response({"ok": False, "error": text}, status=status)


@web.middleware
async def auth(request, handler):
    if request.headers.get("Authorization") != f"Bearer {TOKEN}":
        return fail("unauthorized", 401)
    try:
        return await handler(request)
    except Exception as e:  # the editor reads the reason
        return fail(f"{type(e).__name__}: {e}", 500)


async def status(request):
    if not await client.is_user_authorized():
        return ok({"authorized": False})
    me = await client.get_me()
    return ok({"authorized": True, "me": {"id": me.id, "name": utils.get_display_name(me),
                                          "username": me.username, "phone": me.phone}})


async def login_code(request):
    body = await request.json()
    sent = await client.send_code_request(body["phone"])
    login.update(phone=body["phone"], hash=sent.phone_code_hash)
    return ok({"sent": True})


async def login_sign_in(request):
    body = await request.json()
    try:
        if "password" in body:
            await client.sign_in(password=body["password"])
        else:
            await client.sign_in(phone=login["phone"], code=str(body["code"]),
                                 phone_code_hash=login["hash"])
    except SessionPasswordNeededError:
        return ok({"authorized": False, "needs": "password"})
    await client.get_dialogs(limit=200)
    return ok({"authorized": True})


async def dialogs(request):
    limit = int(request.query.get("limit", "50"))
    archived = request.query.get("archived") == "1"
    rows = []
    async for d in client.iter_dialogs(limit=limit, archived=archived):
        last = d.message
        rows.append({
            "id": d.id,
            "name": d.name,
            "kind": kind_of(d.entity),
            "unread": d.unread_count,
            "mentions": d.unread_mentions_count,
            "pinned": d.pinned,
            "date": iso(d.date),
            "ts": ts(d.date),
            "last": message_json(last, d.name) if last else None,
        })
    return ok(rows)


async def messages(request):
    q = request.query
    ent = await entity(q["chat"])
    before = int(q.get("before", "0"))
    ms = await client.get_messages(ent, limit=int(q.get("limit", "50")), max_id=before)
    return ok([message_json(m) for m in ms])


async def search(request):
    q = request.query
    ent = await entity(q["chat"]) if q.get("chat") else None
    rows = []
    async for m in client.iter_messages(ent, search=q["q"], limit=int(q.get("limit", "30"))):
        chat = await m.get_chat()
        row = message_json(m, utils.get_display_name(chat) if chat else None)
        row["chat_id"] = m.chat_id
        row["chat"] = utils.get_display_name(chat) if chat else ""
        rows.append(row)
    return ok(rows)


async def send(request):
    body = await request.json()
    m = await client.send_message(await entity(body["chat"]), body["text"],
                                  reply_to=body.get("reply_to"))
    return ok(message_json(m))


async def read(request):
    body = await request.json()
    await client.send_read_acknowledge(await entity(body["chat"]))
    return ok()


async def updates(request):
    after = int(request.query.get("after", "0"))
    timeout = min(float(request.query.get("timeout", "25")), 50)
    async with changed:
        try:
            await asyncio.wait_for(changed.wait_for(lambda: seq > after), timeout)
        except asyncio.TimeoutError:
            pass
    return ok([e for s, e in log if s > after], seq=seq)


async def download(request):
    q = request.query
    m = await client.get_messages(await entity(q["chat"]), ids=int(q["id"]))
    if not m or not m.media:
        return fail("no media", 404)
    os.makedirs(FILES, exist_ok=True)
    path = await m.download_media(file=FILES + "/")
    return web.FileResponse(path)


@client.on(events.NewMessage)
async def on_message(event):
    global seq
    chat = await event.get_chat()
    name = utils.get_display_name(chat) if chat else ""
    m = event.message
    if m.sender is None:
        await m.get_sender()
    async with changed:
        seq += 1
        log.append((seq, {"type": "message", "seq": seq, "chat_id": event.chat_id,
                          "chat": name, "kind": kind_of(chat), "message": message_json(m, name)}))
        del log[:-1000]
        changed.notify_all()


async def main():
    os.makedirs(HOME, exist_ok=True)
    await client.connect()
    if await client.is_user_authorized():
        await client.get_dialogs(limit=200)
    app = web.Application(middlewares=[auth])
    app.add_routes([
        web.get("/status", status), web.post("/login/code", login_code),
        web.post("/login/sign_in", login_sign_in), web.get("/dialogs", dialogs),
        web.get("/messages", messages), web.get("/search", search),
        web.post("/send", send), web.post("/read", read),
        web.get("/updates", updates), web.get("/download", download),
    ])
    runner = web.AppRunner(app)
    await runner.setup()
    await web.TCPSite(runner, HOST, PORT).start()
    print(f"telegram bridge on {HOST}:{PORT}", flush=True)
    await asyncio.Event().wait()


if __name__ == "__main__":
    asyncio.run(main())
