# todo
A minimalist pure todo list for terminal which could also turns to be impure as you wish.

## usage
```
Usage: todo [-s|--source FILE] COMMAND

Available options:
  -s,--source FILE         Specify a storage file. (Default) Find .todo from
                           local directory to parent until the user home.
  -h,--help                Show this help text

Available commands:
  add                      Add a todo task
  list                     List all unfinished tasks with index
  fine                     Finish a task specify by index
  rollback                 Rollback permanently, can't do this after gc
  gc                       Collect garbage, which would clean all unused history
  init                     Initialize a storage file .todo in the local
                           directory if not exist
  version                  Print version
```

## auto completion

- for `bash`

``` bash
todo --bash-completion-script `which todo`| sudo tee /etc/bash_completion.d/todo
```

- for `zsh` use `--zsh-completion-script`
- for `fish` sue `--fish-completion-script`

## Forget it
Keeping pure might mean to cost a lot, it's immutable and there is a free time machine which could bring you came back to the past.

Then, you might be favour to turn to be impure, and silently forget everything you had done.

``` bash
alias todo='sh -c '\''todo "$@" ; todo gc > /dev/null 2>&1 '\'' _'
```
