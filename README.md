# pollsterl
Yet another a poll bot for Discord.

**Note:** This repo is still a very early WIP so use it carefully.

## Design
pollsterl is written in Erlang, and is structured as an OTP release with support for live debugging and hot version upgrades.

(TBC)

## Usage
WIP - The below list isn't actually implemented, it simply represents the vision I have in mind
```bash
# Format:
> !poll <command> [args...]

# Print the usage info
> !poll !help
> !poll !info

# Starts a poll with a certain subject
# You can also omit !start for convenience
> !poll !start <subject>

# Omitting !start is a shortcut
# Start a basic poll with 3 options: yes, no, maybe/undecided
> !poll anyone up for some gr?
> !poll "Has anyone really been far even as decided to use even go want to do look more like?"

# Start a poll with custom options
> !poll "Which starter pokemon is your favorite?" Charmander Bulbasaur Squirtle
> !poll "Which faction do you prefer?" NCR "Caesar's Legion" "Mr. House" "I go about my own way"

# Stop/finish/cancel polls
> !poll !stop <poll id>
> !poll !stop <poll id 1> <poll id 2> ...
> !poll !stop last
> !poll !stop all
# When <poll id> is omitted, it refers to the last poll created on the channel
> !poll !stop

# Set an expiry date for a poll
> !poll !expire <poll id> 1 hour
> !poll !expire <poll id> tomorrow
> !poll !expire <poll id> "2019-08-16T13:44:51+0300"
# When <poll id> is omitted, it refers to the last poll created on the channel
> !poll !expire 1 hour
```

Other valid prefixes are `!pollster` and `!pollsterl`.

## TODO
- Implement core functionality
    - Use proper seq no's for Discord heartbeats
    - Start and end polls
- Actual release
    - Deployment
    - Live debugging & hot version upgrades
- (Future) Implement a DSL-based rule engine to allow users to define custom behaviors without having to modify the code

## License
MIT
