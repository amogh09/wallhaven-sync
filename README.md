# wallhaven-sync
`wallhaven-sync` is a command-line tool for syncing wallpapers from a Wallhaven collection to a local directory on your computer. `wallhaven-sync` can also detect wallpapers in your local sync directory that are not a part of the collection anymore and delete them.

## Installation
### Using prebuilt binaries
Prebuilt binaries are provided on [Github Releases](https://github.com/amogh09/wallhaven-sync/releases) for Linux and MacOS platforms. Download the relevant binary for your platform from the Releases page and they should be ready for use.

### Installing from source
Follow the steps below to build `wallhaven-sync` from source. You will need a working [cabal](https://www.haskell.org/cabal/download.html) tool with GHC 9.2.4 for the build.

1. Download the source from one of the [releases](https://github.com/amogh09/wallhaven-sync/releases).
1. Inside the source directory, invoke `cabal install`.
1. `wallhaven-sync` will be installed in your `.cabal/bin` directory.

## Usage
`wallhaven-sync` requires your Wallhaven API Key. 

1. Log into your [Wallhaven](https://wallhaven.cc/) account and then open the [Account Settings](https://wallhaven.cc/settings/account) page. 
1. Your API key will be shown under "API Key" section as shown in the image below.
![Wallhaven API Key](/assets/images/api_key.png "Wallhaven API Key")

Usage instructions can be printed by invoking `wallhaven-sync --help`. The help output is reproduced below.

```
wallhaven-sync

Usage: wallhaven-sync [--wallpaper-dir DIRECTORY] [--delete-unliked]
                      --wallhaven-username USERNAME --wallhaven-api-key API_KEY
                      [--collection-label LABEL] [--debug]

  Sync wallpapers from a Wallhaven collection

Available options:
  --wallpaper-dir DIRECTORY
                           Directory where wallpapers will be saved
                           (default: "/Users/home/wallpapers")
  --delete-unliked         Delete unliked wallpapers
  --wallhaven-username USERNAME
                           Wallhaven username
  --wallhaven-api-key API_KEY
                           Wallhaven API key
  --collection-label LABEL Label of the collection to sync (default: "Default")
  --debug                  Debug mode
  -h,--help                Show this help text
```

### Options
| Option | Description | Default value | Required |
| ------ | ----------- | ------------- | -------- |
| `--wallpaper-dir` | Local directory to use for downloading the wallpapers. | `/Users/home/wallpapers` | No |
| `--wallhaven-username` | Username of the collection owner. | | Yes |
| `--collection-label` | Name of the collection to download. | Default | No |
| `wallhaven-api-key` | Your wallhaven API key. | | Yes |
| `--delete-unliked` | If this flag is present then `wallhaven-sync` will delete wallpapers that are in the local directory but not in the specified collection. This option is useful for deleting wallpapers that were unliked from your collection. | NA | No |

### Example usage
* For downloading your Wallhaven favorites to default directory

`wallhaven-sync --wallhaven-username USERNAME --wallhaven-api-key API_KEY`

* For downloading your wallhaven favorites to ~/wallhaven directory

`wallhaven-sync --wallhaven-username USERNAME --wallhaven-api-key API_KEY --wallpaper-dir ~/wallhaven`

* For syncing (deleting unliked wallpapers from local directory and downloading wallpapers from the collection) Wallhaven favorites to default directory

`wallhaven-sync --wallhaven-username USERNAME --wallhaven-api-key API_KEY --delete-unliked`

* For downloading a collection named "nature" under account "my-account" to default directory

`wallhaven-sync --wallhaven-username my-account --collection-label nature --wallhaven-api-key API_KEY`
