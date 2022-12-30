# wallhaven-sync
wallhaven-sync is a command-line tool for syncing your favorite wallpapers from Wallhaven's Favorites page to a local directory on your computer. wallhaven-sync can also detect wallpapers in your local sync directory that are not favorite on Wallhaven anymore and delete them.

## Usage
`wallhaven-sync` requires your Wallhaven authentication token in order to fetch your favorite wallpapers from Wallhaven. The tool will look for the authentication token in `WALLHAVEN_COOKIE` environment variable. You need to set this environment variable with your authentication token value. 

Usage instructions can be printed by invoking `wallhaven-sync --help`. The help output is reproduced below.

```
wallhaven-sync

Usage: wallhaven-sync [--wallpaper-dir DIRECTORY] [--num-parallel-downloads NUM]
                      [--num-retries NUM] [--retry-delay NUM] [--delete-unliked]

  Sync wallpapers from Wallhaven favorites

Available options:
  --wallpaper-dir DIRECTORY
                           Directory where wallpapers will be saved
                           (default: "/Users/home/wallpapers")
  --num-parallel-downloads NUM
                           Number of wallpapers to download in parallel
                           (default: 5)
  --num-retries NUM        Number of retries to perform when downloading a
                           wallpaper (default: 5)
  --retry-delay NUM        Number of seconds to wait between retries
                           (default: 3)
  --delete-unliked         Delete unliked wallpapers
  -h,--help                Show this help text
  ```

All options have sensible defaults and invoking `wallhaven-sync` command will download all your favorite wallpapers from Wallhaven to `/Users/home/wallpapers` directory. The directory will be created if it does not exist.
