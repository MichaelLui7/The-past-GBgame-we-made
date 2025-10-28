#include <stdint.h>

// how a 2*2 object looks like
// +---+---+
// | 0 | 1 |
// +---+---+
// | 2 | 3 |
// +---+---+

// size of objects
#define playerSize (2 * 2)
#define bulletSize (1 * 1)
#define killerSize (1 * 2)
#define numberSize (1 * 1)

// Where objs are located in tile
#define tileNumber 0  // 1*1 blocks
#define tilePlayer 10 // 2*2 blocks
#define tileBullet 14 // 1*1 blocks
#define tileKiller 15 // 1*2 blocks

// Where objs are located in OAM
#define oamPlayer 0   // 2*2 blocks
#define oamKiller 4   // 1*2 blocks 3 objs
#define oamScores 10  // 1*1 blocks 4 objs
#define oamBullets 14 // 1*1 blocks ? objs

// define locations
#define ground 120
#define west 8
#define east 160
#define north 16
#define south 144 + 8

extern uint8_t readkeys();
extern void copyShadowOAMtoOAM();
extern uint8_t rand();

struct Obj
{
    uint8_t y;
    uint8_t x;
    uint8_t tile;
    uint8_t flags;
} shadowOAM[40];
int8_t acceleration;
int8_t speed;
uint8_t bullet_num;
uint8_t score[6];
extern void *tiles[];

void init_obj()
{
    acceleration = 0;
    speed = 0;
    bullet_num = 0;
    score[0] = 0;
    score[1] = 0;
    score[2] = 0;
    score[3] = 0;
    score[4] = 0;
    score[5] = 0;

    shadowOAM[oamPlayer + 0].y = ground;
    shadowOAM[oamPlayer + 0].tile = tilePlayer + 0;
    shadowOAM[oamPlayer + 1].y = ground;
    shadowOAM[oamPlayer + 1].tile = tilePlayer + 1;
    shadowOAM[oamPlayer + 2].y = ground + 8;
    shadowOAM[oamPlayer + 2].tile = tilePlayer + 2;
    shadowOAM[oamPlayer + 3].y = ground + 8;
    shadowOAM[oamPlayer + 3].tile = tilePlayer + 3;

    shadowOAM[oamKiller + 0].x = 46;
    shadowOAM[oamKiller + 1].x = 46;
    shadowOAM[oamKiller + 2].x = 84;
    shadowOAM[oamKiller + 3].x = 84;
    shadowOAM[oamKiller + 4].x = 122;
    shadowOAM[oamKiller + 5].x = 122;
    shadowOAM[oamKiller + 0].tile = tileKiller + 0;
    shadowOAM[oamKiller + 1].tile = tileKiller + 1;
    shadowOAM[oamKiller + 2].tile = tileKiller + 0;
    shadowOAM[oamKiller + 3].tile = tileKiller + 1;
    shadowOAM[oamKiller + 4].tile = tileKiller + 0;
    shadowOAM[oamKiller + 5].tile = tileKiller + 1;

    shadowOAM[oamScores + 0].y = north + 64;
    shadowOAM[oamScores + 1].y = north + 64 + 10;
    shadowOAM[oamScores + 2].y = north + 64 + 20;
    shadowOAM[oamScores + 3].y = north + 64 + 30;
    shadowOAM[oamScores + 0].x = east + 8;
    shadowOAM[oamScores + 1].x = east + 8;
    shadowOAM[oamScores + 2].x = east + 8;
    shadowOAM[oamScores + 3].x = east + 8;
}
extern uint8_t left;
extern uint8_t right;
extern uint8_t up;
extern uint8_t down;
extern uint8_t B;
extern uint8_t A;

uint8_t key_state = 0;

void shoot()
{
    if (bullet_num > 20)
        return;
    shadowOAM[oamBullets + bullet_num].y = shadowOAM[oamPlayer + 0].y;
    shadowOAM[oamBullets + bullet_num].x = shadowOAM[oamPlayer + 0].x + 6;
    shadowOAM[oamBullets + bullet_num].tile = tileBullet;
    bullet_num++;
}

void sync_player()
{
    uint8_t a = shadowOAM[oamPlayer + 0].x;
    shadowOAM[oamPlayer + 1].x = a + 8;
    shadowOAM[oamPlayer + 2].x = a;
    shadowOAM[oamPlayer + 3].x = a + 8;
    return;
}
void sync_killer()
{
    shadowOAM[oamKiller + 1].y = shadowOAM[oamKiller + 0].y + 8;
    shadowOAM[oamKiller + 3].y = shadowOAM[oamKiller + 2].y + 8;
    shadowOAM[oamKiller + 5].y = shadowOAM[oamKiller + 4].y + 8;
    return;
}
void update_bullet()
{
    struct Obj *last_bullet = shadowOAM + oamBullets + sizeof(struct Obj) * bullet_num;

    struct Obj *this = shadowOAM + oamBullets;
    for (int i = 1; i <= bullet_num;)
    {
        if (this->y > south)
        {
            uint8_t b = last_bullet->y;
            uint8_t c = last_bullet->x;
            this->y = b;
            this->x = c;
            bullet_num -= 1;
            last_bullet -= sizeof(struct Obj);

            continue;
        }
        this->y -= 2;
        struct Obj *killer = shadowOAM + oamKiller;
        for (int j = 0; j < 3; j++)
        {
            if (this->x >= killer->x - 8 &&
                this->x <= killer->x + 8 &&
                killer->y + 8 >= this->y)
            {
                killer->y = south;
                this->y = 0;
                break;
            }
            killer += 2 * sizeof(struct Obj);
        }
        i++;
        this += sizeof(struct Obj);
    }
    return;
}

void move_and_shoot()
{
    read_keys();
    if (key_state & left)
    {
        acceleration = -2;
    }
    else if (key_state & right)
    {
        acceleration = 2;
    }
    else if (key_state & (A | B))
    {
        shoot();
    }
    speed += acceleration;
    acceleration = 0;
    shadowOAM[oamPlayer + 0].x += speed;
    if (shadowOAM[oamPlayer + 0].x < west)
    {
        shadowOAM[oamPlayer + 0].x = west;
        speed = (-speed) / 2;
    }
    if (shadowOAM[oamPlayer + 0].x > east)
    {
        speed = (-speed) / 2;
        shadowOAM[oamPlayer + 0].x = east;
    }
    return sync_player();
}
void update_killers()
{
    for (int i = 0; i < 3; i++)
    {
        if (shadowOAM[oamKiller + i * 2].y <= north)
        {
            shadowOAM[oamKiller + i * 2].y = south | rand();
        }
        shadowOAM[oamKiller + i].y += 1;
    }
    return sync_killer();
}
void death_or_score()
{
    score[0] += 1;
    for (int i = 0; i < 3; i++)
    {
        if (shadowOAM[oamKiller + i * 2].y == ground)
        {
            return clear_score();
        }
    }
    return;
}
void clear_score()
{
    score[0] = 0;
    score[1] = 0;
    score[2] = 0;
    score[3] = 0;
    score[4] = 0;
    score[5] = 0;
    return;
}
void show_score()
{
    if (score[0] > 60)
    {
        score[1] += 1;
        score[0] -= 60;
    }
    for (int i = 1; i < 5; i++)
    {
        if (score[i] >= 10)
        {
            score[i] = 0;
            score[i + 1] += 1;
        }
    }

    shadowOAM[oamScores + 0].tile = tileNumber + score[1];
    shadowOAM[oamScores + 1].tile = tileNumber + score[2];
    shadowOAM[oamScores + 2].tile = tileNumber + score[3];
    shadowOAM[oamScores + 3].tile = tileNumber + score[4];

    return;
}
int main()
{
    init_obj();
    while (1)
    {
        move_and_shoot();
        update_bullet();
        update_killers();
        death_or_score();
        show_score();
        copyShadowOAMtoOAM();
    }
}
