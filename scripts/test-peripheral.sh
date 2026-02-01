#!/bin/bash
# Start fuclaude peripheral (fresh session, connected to Agency)
cd "$(dirname "$0")/.."
exec ./scripts/fuclaude-peripheral.ts
