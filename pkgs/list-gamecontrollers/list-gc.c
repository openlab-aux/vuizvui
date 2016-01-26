#include <SDL.h>

void dump_guid(SDL_Joystick *js) {
    SDL_JoystickGUID guid;
    const char *name;
    char guidstr[33];

    guid = SDL_JoystickGetGUID(js);
    name = SDL_JoystickName(js);
    SDL_JoystickGetGUIDString(guid, guidstr, sizeof(guidstr));

    printf("%s: %s\n", name, guidstr);
}

int main()
{
    int i;
    SDL_Joystick *js;

    SDL_Init(SDL_INIT_JOYSTICK);
    atexit(SDL_Quit);

    for (i = 0; i < SDL_NumJoysticks(); ++i) {
        if ((js = SDL_JoystickOpen(i)) != NULL) {
            dump_guid(js);
            SDL_JoystickClose(js);
        }
    }

    return EXIT_SUCCESS;
}
