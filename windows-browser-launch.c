#include <windows.h>
#include <shellapi.h>

void launchBrowser(char *url)
{
    ShellExecute(NULL, "open", url, NULL, NULL, SW_SHOWNORMAL);
}
