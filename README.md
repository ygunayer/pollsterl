# pollsterl
Yet another a poll bot for Discord.

**Note:** This repo is still a very early WIP so use it carefully.

## Design
pollsterl is written in Erlang, and is structured as an OTP release with support for live debugging and hot version upgrades.

(TBC)

## Usage
The basic command syntax is:

`!poll <!command> [args...]`

See subsections for more details.

**Hint:** Other valid prefixes are `!pollster` and `!pollsterl`.

### !help
You can use `!poll !help` or `!poll !info` to print the general help text.
```
!poll !help
```
```
!poll !info
```

To request help on a specific command or topic, enter one of the following:
- `!poll !help !start`: Starting polls
- `!poll !help !stop`: Stopping polls
- `!poll !help !expire`: Managing expiration dates of polls

### !start
You can use the `!start` command to start a poll:
```
!poll !start <subject>
```

**Hint**: Each poll has a unique ID that can later be used to refer to it.

You can also omit the `!start` command to quickly start a poll:
```
!poll anyone up for some gr?
```
```
!poll "Has anyone really been far even as decided to use even go want to do look more like?"
```

By default, all polls start with the basic yes/no/undecided options, but you can also specify your own (up to 36):

```
!poll "Which starter pokemon is your favorite?" Charmander Bulbasaur Squirtle
```
```
!poll "Which faction do you prefer?" NCR "Caesar's Legion" "Mr. House" "I go about my own way"
```

### !stop
You can use the `!stop` command to stop an ongoing command.

The default usage of the stop command is:
```
!poll !stop <poll id>
```

You can also specify multiple poll IDs
```
!poll !stop <poll id 1> <poll id 2> ...
```

The word `last` can be used to quickly refer to the latest poll that was created on the current channel
```
!poll !stop last
```

...or omit any ID or keyword to refer to it
```
!poll !stop
```

To stop all ongoing polls in the *current channel*, use the keyword `here`
```
!poll !stop here
```

Or, to stop all polls globally, use the keyword `all`
```
!poll !stop all
```

### !expire - (WORK IN PROGRESS)
The `!expire` command can be used to set an expiration date for a poll that when reach will automatically cause the poll to be closed.

You can specify the expiration date as a relative time
`!poll !expire <poll id> 1 hour`
`!poll !expire <poll id> tomorrow`

...or a specific date
`!poll !expire <poll id> "2019-08-16T13:44:51+0300"`

You can omit the poll ID to refer to the latest poll created in the current channel
`!poll !expire 1 hour`


## TODO
- EDoc
- Implement core functionality
    - Use proper seq no's for Discord heartbeats
    - Start and end polls
- Persist poll data with `mnesia`
- Use dialyzer
- Implement more tests
- Maybe use `ct` instead of `eunit`
- Actual release
    - Deployment
- Maybe use `recon`
- (Future) Implement a DSL-based rule engine to allow users to define custom behaviors without having to modify the code

## License
MIT
