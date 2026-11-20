# compos-telegram

Telegram for [compos](https://github.com/compos-dev), in two parts.

**The bot gateway** (`telegram.scm`) lets approved people talk to a compos agent through a Telegram bot, the way the openclaw and hermes gateways do. A stranger gets a pairing code; `M-x telegram-allow` approves it. While the agent works, Telegram shows typing, a 👀 reaction and a progress message of its tools; the reply arrives formatted, as a reply to the message that asked. Photos, files and voice notes are saved and their paths given to the agent. In a group the bot answers only an @mention. Commands: `/new`, `/stop`, `/status`, `/help`. `telegram-chat-routes` says which compos group's chat answers whom.

**The account app** (`telegram-app.scm`, `M-x telegram`) is your own Telegram account: every chat with its unread count, a page per conversation, `r` to send as yourself, `R` to mark read, `s` to search every chat. New messages arrive live, show a notice, and are published as `telegram-account:CHAT-ID` events for agents.

## Install

```scheme
(use-package 'telegram 'repo "compos-dev/compos-telegram")
```

### The bot

1. Make a bot with @BotFather and put its token in `telegram-bot-token` (or `TELEGRAM_BOT_TOKEN`).
2. `M-x telegram-start`. It starts again at every boot.
3. Message the bot; approve your own pairing code with `(telegram-approve! CODE 'owner)`, and staff with `M-x telegram-allow`.

### The account

The account is reached through `bridge/bridge.py`, a small Telethon service. Run it on a machine that stays on:

1. Get an `api_id` and `api_hash` at my.telegram.org, and pick a long random bridge token.
2. Copy `bridge/` to the machine, write `~/telegram-bridge/env` with `TELEGRAM_API_ID`, `TELEGRAM_API_HASH`, `TELEGRAM_BRIDGE_TOKEN`, `BRIDGE_HOST` and `BRIDGE_PORT`, and install `bridge/telegram-bridge.service` as a systemd user unit. It runs with `uv run --script`, which fetches Telethon and aiohttp.
3. In compos set `telegram-bridge-url` (`http://HOST:PORT`) and `telegram-bridge-token`, then `M-x telegram-login`: your phone number, the code Telegram sends, and the two-step password if you have one.
4. `M-x telegram`.

Keep the bridge on a private network: it holds a full login to the account.
