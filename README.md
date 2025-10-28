# The-past-GBgame-we-made
It is a project of designing the GBgame with assembly
# Final Project
Songnian He 999015068
Xinyuan Lv 999015324
Zixuan Huang 999014145

## How to Play

Press `Left`/`Right` Arrow to move the Cannon Gun, press Gameboy `A` button(default computer keyboard `S`) to shoot.
Recommend using Xbox Series Controller to play. 

check the video.

## How it Works
### Execution Loop
We do `init_obj` defore `MainLoop` to put everything in place.
Inside `MainLoop`, we do following thing:
 1. `move_and_shoot` do exactely what its name saying, `readkey` was called in it.
 2. `update_bullet` move bullet up and check if it hit the killer or move outside the map, if it does, remove bullet and move killer out of map, and give 10 points.
 3. `update_killers` do much less thing: move it down and move it out of map if it reach `ground`.
 4. `death_or_score` if any killer reach `ground` clean the socre and add 1 point per second.
 5. `copyShadowOAMtoOAM` you know it.

### Variables
 * `speed`: how fast it will move, but `speed>>2` is acctully used to make it more soother. `speed` will be cut by half once it reach the Left/Right bound.
 * `bullet_num` & `last_bullet`: second one is a pointer to last bullet in `shadowOAM`.
 * `score[1..6]` from 0 to 9, the score to display.
 * `score[0]` from 0 to 63, as a buffer to make the number we dispaly growth 1(?) per second.

### Constant 
 * `tile***`: where should we load tile
 * `oam****`: where a object is in `shadowOAM`
 * `ground` : base line of game.

### PS

When we move a killer out of map, that is we set y coord of it to `southlimit | rand()`, that is a random place below the map, and keep adding it to overflow, then it will appear again at the top of map.

When a bullet is going to be removed, we swap it with `*last_bullet` and then reduce `bullet_num` and move the pointer forward.

### For detail, check `obj.c` as pseudocode
