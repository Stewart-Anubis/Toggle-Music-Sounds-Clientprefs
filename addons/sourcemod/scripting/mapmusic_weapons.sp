/*
 * =============================================================================
 * File:		  mapmusic_weapons.sp
 * Type:		  Base
 * Description:   Plugin's base file.
 *
 * Copyright (C)   Anubis Edition. All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 */
#pragma semicolon 1
#include <sdkhooks>
#include <dhooks>
#include <regex>
#include <clientprefs>
#include <smutils>
#include <csgocolors_fix>

#undef REQUIRE_EXTENSIONS
#include <SoundLib>
#include <soundlib2>
#define REQUIRE_EXTENSIONS

#pragma newdecls required

public Plugin myinfo =
{
	name        = "Map Music Weapons Controller",
    author      = "Anubis",
    description = "",
    version     = "1.0",
    url         = "https://github.com/Stewart-Anubis"
};



static StringMap g_smSourceEnt;
static StringMap g_smCtChannel;
static StringMap g_smSndCommon;
static StringMap g_smRecentSnd;
static StringMap g_smSndVolume;
static StringMap g_smSndLength;

static int g_iChannel;
static int g_iNumRound;
static int g_iSTSound;

static bool g_bWeapon[MAXPLAYERS+1] = {false, ...};
static bool g_bHooked;
static bool g_bBGMply[MAXPLAYERS+1] = {false, ...};
static float g_fBGMVol[MAXPLAYERS+1] = {1.0, ...};

static Handle g_ckDisable = INVALID_HANDLE;
static Handle g_ckBGMVole = INVALID_HANDLE;
static Handle g_ckWeapon = INVALID_HANDLE;

static ConVar g_cvar_mapmusic_min_length = null;

static Handle hAcceptInput;
static Regex regPattern;
static RegexError regError;

static int g_pSoundLib = 0;

public APLRes AskPluginLoad2(Handle myself, bool late, char[] error, int err_max)
{
	RegPluginLibrary("MapMusic");

	CreateNative("MapMusic_GetVolume", Native_GetVolume);
	CreateNative("MapMusic_SetVolume", Native_SetVolume);
	CreateNative("MapMusic_GetWSounds", Native_GetWSounds);
	CreateNative("MapMusic_SetWSounds", Native_SetWSounds);
	CreateNative("MapMusic_GetStatus", Native_GetStatus);
	CreateNative("MapMusic_SetStatus", Native_SetStatus);

	__ext_SoundLib_SetNTVOptional();
	__ext_soundlib2_SetNTVOptional();

	return APLRes_Success;
}

public int Native_GetVolume(Handle myself, int numParams)
{
    return GetVolume(GetNativeCell(1));
}

public int Native_SetVolume(Handle myself, int numParams)
{
    SetVolume(GetNativeCell(1), GetNativeCell(2));
}

public int Native_GetWSounds(Handle myself, int numParams)
{
    return g_bWeapon[GetNativeCell(1)];
}

public int Native_SetWSounds(Handle myself, int numParams)
{
    SetStatusWepons(GetNativeCell(1), GetNativeCell(2));
}

public int Native_GetStatus(Handle myself, int numParams)
{
    return g_bBGMply[GetNativeCell(1)];
}

public int Native_SetStatus(Handle myself, int numParams)
{
    SetStatus(GetNativeCell(1), GetNativeCell(2));
}

public void OnPluginStart()
{
	g_smSourceEnt = new StringMap();
	g_smCtChannel = new StringMap();
	g_smSndCommon = new StringMap();
	g_smRecentSnd = new StringMap();
	g_smSndVolume = new StringMap();
	g_smSndLength = new StringMap();

	char preError[256];
	char prePattern[256] = "(([-_a-zA-Z0-9]+[/]?)+[.][a-zA-Z0-9]{3})";
	regPattern = CompileRegex(prePattern, PCRE_CASELESS, preError, 256, regError);
	if(regError != REGEX_ERROR_NONE)
		SetFailState("Regex Error: [%d] %s", view_as<int>(regError), preError);

	char conf[128];
	switch(GetEngineVersion())
	{
		case Engine_CSGO:
		{
			conf = "sdktools.games\\engine.csgo";
			AddTempEntHook("Shotgun Shot", CSS_Hook_ShotgunShot);
		}
		case Engine_CSS:
		{
			conf = "sdktools.games\\engine.css";
			AddTempEntHook("Shotgun Shot", CSS_Hook_ShotgunShot);
		}
		case Engine_TF2:
		{
			conf = "sdktools.games\\engine.tf";
			AddTempEntHook("Shotgun Shot", CSS_Hook_ShotgunShot);
		}
		case Engine_Left4Dead2: 
		{
			conf = "sdktools.games\\engine.Left4Dead2";
			AddTempEntHook("FireBullets", DODS_Hook_FireBullets);
		}
		default: SetFailState("Game Engine ??");
	}

	AddNormalSoundHook(Hook_NormalSound);
	Handle GameConf = LoadGameConfigFile(conf);

	if(GameConf == null)
		SetFailState("Why you no has gamedata?");

	if(!HookEventEx("round_poststart", Event_PostRoundStart))
		SetFailState("Failed to Hook Event \"round_poststart\".");

	int offset = GameConfGetOffset(GameConf, "AcceptInput");
	hAcceptInput = DHookCreate(offset, HookType_Entity, ReturnType_Bool, ThisPointer_CBaseEntity, AcceptInput);
	if(hAcceptInput == null)
		SetFailState("Failed to DHook \"AcceptInput\".");

	DHookAddParam(hAcceptInput, HookParamType_CharPtr);
	DHookAddParam(hAcceptInput, HookParamType_CBaseEntity);
	DHookAddParam(hAcceptInput, HookParamType_CBaseEntity);
	DHookAddParam(hAcceptInput, HookParamType_Object, 20);
	DHookAddParam(hAcceptInput, HookParamType_Int);

	delete GameConf;

	RegConsoleCmd("sm_mapmusic",    Command_Music,      "Brings up the music menu");
	RegConsoleCmd("sm_music",    Command_Music,      "Brings up the music menu");
	RegConsoleCmd("sm_volume", Command_Volume, "Brings volume menu");
	RegConsoleCmd("sm_stopsound", Command_StopSound, "Toggle hearing weapon sounds");
	RegConsoleCmd("sm_startsound",  Command_StartSound, "Start weapon sounds");
	RegConsoleCmd("sm_stopmusic",   Command_StopMusic,  "Toggles map music");
	RegConsoleCmd("sm_startmusic",  Command_StartMusic, "Start map music");
	RegConsoleCmd("sm_playmusic",   Command_StartMusic, "Start map music");

	g_cvar_mapmusic_min_length = CreateConVar("mapmusic_min_length", "10.0", "How long required length for it will be music files.", _, true, 0.0);

	g_ckDisable = RegClientCookie("mapmusic_disable", "Disable Map Music", CookieAccess_Private);
	g_ckBGMVole = RegClientCookie("mapmusic_volume",  "Map Music Volume",  CookieAccess_Private);
	g_ckWeapon = RegClientCookie("mapmusic_stopsound", "Toggle Weapon Sounds", CookieAccess_Private);

	SetCookieMenuItem(PrefMenu, 0, "Map Music");

	for(int i = 1; i <= MaxClients; i++)
	{
		if (IsClientInGame(i) && !IsFakeClient(i)) OnClientCookiesCached(i);
	}

	CheckLibrary();

	AutoExecConfig(true, "Mapmusic_Weapons");
}

static void CheckLibrary()
{
    if(LibraryExists("soundlib2"))
        g_pSoundLib = 2;
    else if(LibraryExists("soundlib"))
        g_pSoundLib = 1;
    else
        SetFailState("This plugin require 'soundlib2.ext' or 'soundlib.ext' running.");
}

public void OnLibraryAdded(const char[] name)
{
    CheckLibrary();
}

public void OnLibraryRemoved(const char[] name)
{
    CheckLibrary();
}

public void OnMapStart()
{
	LoadTranslations("Mapmusic_Weapons.phrases");
	g_smSourceEnt.Clear();
	g_smCtChannel.Clear();
	g_smSndCommon.Clear();
	g_smRecentSnd.Clear();
	g_smSndVolume.Clear();
	g_smSndLength.Clear();

	g_iChannel = SNDCHAN_USER_BASE - 75;

	g_iNumRound = 0;

	g_iSTSound = FindStringTable("soundprecache");
	if(g_iSTSound == INVALID_STRING_TABLE)
		SetFailState("Failed to find string table \"soundprecache\".");
}

public void OnClientDisconnect_Post(int client)
{
	g_bBGMply[client] = false;
	g_fBGMVol[client] = 1.0;
	g_bWeapon[client] = false;
	CheckHooks();
}

public void OnClientCookiesCached(int client)
{
	char buffer[3][8];

	GetClientCookie(client, g_ckDisable, buffer[0], 8);
	g_bBGMply[client] = (buffer[0][0] == '\0') ? false : view_as<bool>(StringToInt(buffer[0]));

	GetClientCookie(client, g_ckBGMVole, buffer[1], 8);
	g_fBGMVol[client] = (buffer[1][0] == '\0') ? 1.0   : StringToFloat(buffer[1]);

	GetClientCookie(client, g_ckWeapon, buffer[2], 8);
	g_bWeapon[client] = (buffer[2][0] == '\0') ? false : view_as<bool>(StringToInt(buffer[2]));
	CheckHooks();
}

public void Event_PostRoundStart(Event event, const char[] name, bool dontBroadcast)
{
	g_smRecentSnd.Clear();
	g_smSndVolume.Clear();

	g_iNumRound++;

	for(int client = 1; client <= MaxClients; client++)
		if(ClientIsValid(client))
		{
			/*
			ClientCommand(client, "snd_setsoundparam Music.StartRound.valve_csgo_01     volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartRound_01.valve_csgo_01  volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartRound_02.valve_csgo_01  volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartRound_03.valve_csgo_01  volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartAction.valve_csgo_01    volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartAction_01.valve_csgo_01 volume 0"); 
			ClientCommand(client, "snd_setsoundparam Music.DeathCam.valve_csgo_01       volume 0");
			ClientCommand(client, "snd_setsoundparam Music.LostRound.valve_csgo_01      volume 0");
			ClientCommand(client, "snd_setsoundparam Music.WonRound.valve_csgo_01       volume 0");
			ClientCommand(client, "snd_setsoundparam Music.MVPAnthem.valve_csgo_01      volume 0");
			ClientCommand(client, "snd_setsoundparam Music.MVPAnthem_01.valve_csgo_01   volume 0");

			ClientCommand(client, "snd_setsoundparam Music.StartRound.valve_csgo_02     volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartRound_01.valve_csgo_02  volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartRound_02.valve_csgo_02  volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartRound_03.valve_csgo_02  volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartAction.valve_csgo_02    volume 0");
			ClientCommand(client, "snd_setsoundparam Music.StartAction_01.valve_csgo_02 volume 0"); 
			ClientCommand(client, "snd_setsoundparam Music.DeathCam.valve_csgo_02       volume 0");
			ClientCommand(client, "snd_setsoundparam Music.LostRound.valve_csgo_02      volume 0");
			ClientCommand(client, "snd_setsoundparam Music.WonRound.valve_csgo_02       volume 0");
			ClientCommand(client, "snd_setsoundparam Music.MVPAnthem.valve_csgo_02      volume 0");
			ClientCommand(client, "snd_setsoundparam Music.MVPAnthem_01.valve_csgo_02   volume 0");
			*/
		}
}

void CheckHooks()
{
	bool bShouldHook = false;
	
	for (int i = 1; i <= MaxClients; i++)
	{
		if (g_bWeapon[i])
		{
			bShouldHook = true;
			break;
		}
	}
	
	// Fake (un)hook because toggling actual hooks will cause server instability.
	g_bHooked = bShouldHook;
}

public Action Command_Music(int client, int args)
{
	if(!ClientIsValid(client))
		return Plugin_Handled;

	DisplaySettingsMenu(client);

	return Plugin_Handled;
}

public Action Command_Volume(int client, int args)
{
	if(!ClientIsValid(client))
		return Plugin_Handled;

	DisplayVolumeMenu(client);

	return Plugin_Handled;
}

public Action Command_StopMusic(int client, int args)
{
    if(!ClientIsValid(client))
        return Plugin_Handled;

    if(g_bBGMply[client])
    {
        SetStatus(client, false);
        return Plugin_Handled;
    }

    SetStatus(client, true, true);

    return Plugin_Handled;
}

public Action Command_StopSound(int client, int args)
{
    if(!ClientIsValid(client))
        return Plugin_Handled;

    if(g_bWeapon[client])
    {
        SetStatusWepons(client, false);
        return Plugin_Handled;
    }

    SetStatusWepons(client, true, true);

    return Plugin_Handled;
}

public Action Command_StartMusic(int client, int args)
{
    if(!ClientIsValid(client))
        return Plugin_Handled;

    SetStatus(client, false);

    return Plugin_Handled;
}

public Action Command_StartSound(int client, int args)
{
	if(!ClientIsValid(client))
		return Plugin_Handled;

	SetStatusWepons(client, false);

	return Plugin_Handled;
}

public void PrefMenu(int client, CookieMenuAction actions, any info, char[] buffer, int maxlen)
{
	if(actions == CookieMenuAction_DisplayOption)
	{
		FormatEx(buffer, maxlen, "%T", "Cookie_Menu", client);
	}

	if(actions == CookieMenuAction_SelectOption)
	{
		DisplaySettingsMenu(client);
	}
}

static void DisplaySettingsMenu(int client)
{
	// Create menu handle.
	Menu prefmenu = CreateMenu(PrefMenuHandler, MENU_ACTIONS_DEFAULT);

	// Make client global translations target.
	SetGlobalTransTarget(client);

	char title[256];
	char musicmap_sounds[32];
	char mapvolume[32];
	char weapons_sounds[32];

	// Translate each line into client's language.
	Format(title, sizeof(title), "%t\n ", "Title_Stop_Sounds");

	//Menu Variavel
	Format(musicmap_sounds, sizeof(musicmap_sounds), "%t %t", "Musicmap_Sounds", g_bBGMply[client] ? "OFF" : "ON");
	Format(weapons_sounds, sizeof(weapons_sounds), "%t %t", "WeaponsSounds", g_bWeapon[client] ? "OFF" : "ON");
	Format(mapvolume, sizeof(mapvolume), "%t %i%", "Map_volume", GetVolume(client));

	// Add items to menu.
	prefmenu.SetTitle(title);
	prefmenu.AddItem("musicmap_sounds", musicmap_sounds);
	prefmenu.AddItem("weapons_sounds", weapons_sounds);
	prefmenu.AddItem("mapvolume", mapvolume);

	// Create a "Back" button to the main menu.
	prefmenu.ExitBackButton = true;

	// Display menu to client.
	prefmenu.Display(client, 30);
}

public int PrefMenuHandler(Menu prefmenu, MenuAction actions, int client, int item)
{
	switch(actions)
	{
		case MenuAction_Select:
		{
			char preference[32];
			GetMenuItem(prefmenu, item, preference, sizeof(preference));

			if(StrEqual(preference, "musicmap_sounds"))
			{
				if (g_bBGMply[client]) SetStatus(client, false);
				else SetStatus(client, true);
			}
			if(StrEqual(preference, "weapons_sounds"))
			{
				if (g_bWeapon[client]) SetStatusWepons(client, false);
				else SetStatusWepons(client, true);
			}
			if(StrEqual(preference, "mapvolume"))
			{
				DisplayVolumeMenu(client);
				//SetVolume(client, StringToInt(preference[4]));
				return;
			}

			DisplaySettingsMenu(client);
		}
		case MenuAction_Cancel:
		{
			if(item == MenuCancel_ExitBack)
				ShowCookieMenu(client);
		}
		case MenuAction_End: delete prefmenu;
	}
}

static void DisplayVolumeMenu(int client)
{
	// Create menu handle.
	Menu prefvolmenu = CreateMenu(PrefVolMenuHandler, MENU_ACTIONS_DEFAULT);

	// Make client global translations target.
	SetGlobalTransTarget(client);

	char volumemenutitle[256];

	// Translate each line into client's language.
	Format(volumemenutitle, sizeof(volumemenutitle), "%t\n%t %i%", "Volume Menu Title", "Map_volume", GetVolume(client));

	// Add items to menu.
	prefvolmenu.SetTitle("%s \n ", volumemenutitle);
	prefvolmenu.AddItem("vol_100", "100%");
	prefvolmenu.AddItem("vol_90", "90%");
	prefvolmenu.AddItem("vol_80", "80%");
	prefvolmenu.AddItem("vol_70", "70%");
	prefvolmenu.AddItem("vol_60", "60%");
	prefvolmenu.AddItem("vol_50", "50%");
	prefvolmenu.AddItem("vol_40", "40%");
	prefvolmenu.AddItem("vol_30", "30%");
	prefvolmenu.AddItem("vol_20", "20%");
	prefvolmenu.AddItem("vol_10", "10%");

	// Create a "Back" button to the main menu.
	prefvolmenu.ExitBackButton = true;

	// Display menu to client.
	prefvolmenu.Display(client, 30);
}

public int PrefVolMenuHandler(Menu prefvolmenu, MenuAction actions, int client, int item)
{
	switch(actions)
	{
		case MenuAction_Select:
		{
			char preference[8];
			GetMenuItem(prefvolmenu, item, preference, sizeof(preference));

			if(strncmp(preference, "vol_", 4) == 0)
				SetVolume(client, StringToInt(preference[4]));

			DisplayVolumeMenu(client);
		}
		case MenuAction_Cancel:
		{
			if(item == MenuCancel_ExitBack)
				//ShowCookieMenu(client);
				DisplaySettingsMenu(client);
		}
		case MenuAction_End: delete prefvolmenu;
	}
}

static int GetVolume(int client)
{
    return RoundToCeil(g_fBGMVol[client] * 100);
}

static void SetVolume(int client, int volume)
{
	SetStatus(client, false, false);

	g_fBGMVol[client] = volume * 0.01;

	if(volume <= 0)
		g_fBGMVol[client] = 0.0;

	if(volume > 100)
		g_fBGMVol[client] = 1.0;

	char sValue[8];
	FloatToString(g_fBGMVol[client], sValue, 8);
	SetClientCookie(client, g_ckBGMVole, sValue);

	if(ClientIsValid(client))
	{
		PrintCenterText(client, "%t %i%", "Center_Volume_Set", RoundToCeil(g_fBGMVol[client] * 100));
		CPrintToChat(client, "%t %i%", "Chat_Volume_Set", RoundToCeil(g_fBGMVol[client] * 100));
		ClientUpdateSoundsVolume(client);
		if(g_bBGMply[client] || g_fBGMVol[client] <= 0.0)
			ClientStopSound(client, "", false);
	}
}

static void SetStatus(int client, bool bBlockMapMusic, bool chat = true)
{
	g_bBGMply[client] = bBlockMapMusic;

	char sValue[8];
	IntToString(view_as<int>(g_bBGMply[client]), sValue, 8);
	SetClientCookie(client, g_ckDisable, sValue);

	if(ClientIsValid(client))
	{
		if(chat)
		{
			CPrintToChat(client, "%t %t", "ChatMusicMapSounds", bBlockMapMusic ? "C_OFF" : "C_ON");
			PrintCenterText(client, "%t %t", "CenterMusicMapSounds", bBlockMapMusic ? "OFF" : "ON");
		}
		if(!bBlockMapMusic) ClientUpdateSoundsVolume(client);
		if(g_bBGMply[client] || g_fBGMVol[client] <= 0.0)
			ClientStopSound(client, "", false);
	}
}

static void SetStatusWepons(int client, bool bBlockWepons, bool chat = true)
{
	g_bWeapon[client] = bBlockWepons;

	char sValue[8];
	IntToString(view_as<int>(g_bWeapon[client]), sValue, 8);
	SetClientCookie(client, g_ckWeapon, sValue);
	//OnClientCookiesCached(client);

	if(ClientIsValid(client))
	{
		if(chat)
		{
			CPrintToChat(client, "%t %t", "ChatWeaponsSounds", bBlockWepons ? "C_OFF" : "C_ON");
			PrintCenterText(client, "%t %t", "CenterWeaponsSounds", bBlockWepons ? "OFF" : "ON");
		}
		CheckHooks();
	}
}

public MRESReturn AcceptInput(int entity, Handle hReturn, Handle hParams)
{
    //Abort if the entity is missing
    if(!IsValidEntity(entity))
        return MRES_Ignored;

    char eClassname[32], eCommand[128], eParam[128], soundFile[256];
    int eActivator;

    DHookGetParamString(hParams, 1, eCommand, 128);

    int type = -1;
    float fParam = 0.0;
    type = DHookGetParamObjectPtrVar(hParams, 4, 16, ObjectValueType_Int);

    if(type == 1) 
        fParam = DHookGetParamObjectPtrVar(hParams, 4, 0, ObjectValueType_Float);
    else if(type == 2)
    {
        DHookGetParamObjectPtrString(hParams, 4, 0, ObjectValueType_String, eParam, 128);
        StringToFloatEx(eParam, fParam);
    }

    if(!DHookIsNullParam(hParams, 2))
    {
        eActivator = DHookGetParam(hParams, 2);
        if(eActivator < -1)
            eActivator = -1;
    }
    else
        eActivator = -1;

    GetEntityClassname(entity, eClassname, 32);

    if(strcmp(eClassname, "point_clientcommand", false) == 0)
    {
        //Don't allow client sounds to override this plugin
        if((StrContains(eParam, ".mp3", false) != -1) || (StrContains(eParam, ".wav", false) != -1))
        {
            int matchCount = MatchRegex(regPattern, eParam, regError);
            if(matchCount > 0)
            {
                if(GetRegexSubString(regPattern, 0, soundFile, 256))
                {
                    AddToStringTable(g_iSTSound, FakePrecacheSound(soundFile, true));
                    PrecacheSound(FakePrecacheSound(soundFile, true), false);
                    ClientSendSound(soundFile, eActivator, true);
                }
            }
            DHookSetReturn(hReturn, false);
            return MRES_Supercede;
        }
        return MRES_Ignored;
    }

    GetEntPropString(entity, Prop_Data, "m_iszSound", soundFile, 256);

    float fLength = GetSoundLengthFloat(soundFile);
    if(fLength && fLength < g_cvar_mapmusic_min_length.FloatValue)
        return MRES_Ignored;

    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");

    if(strcmp(eCommand, "PlaySound", false) == 0 || strcmp(eCommand, "FadeIn", false) == 0 || (strcmp(eCommand, "Volume", false) == 0 && (fParam >= 0.1)) || strcmp(eCommand, "ToggleSound", false) == 0)
    {
        int temp;
        bool common = g_smSndCommon.GetValue(soundFile, temp);

        if(!((StrContains(soundFile, ".mp3", false) != -1) || (StrContains(soundFile, ".wav", false) != -1)))
            return MRES_Ignored; //Workaround for client soundscripts (?)

        if(eFlags & 1)
        {
            float curVol;
            if(g_smSndVolume.GetValue(soundFile, curVol) && (strcmp(eCommand, "Volume", false) == 0 || strcmp(eCommand, "ToggleSound", false) == 0))
            {
                if((curVol != fParam || (curVol >= 0.1 && fParam >= 0.1)) && strcmp(eCommand, "Volume", false) == 0)
                {
                    //Different volume but already playing? Ignore
                    DHookSetReturn(hReturn, false);
                    return MRES_Supercede;
                }
                else if(strcmp(eCommand, "ToggleSound", false) == 0)
                {
                    //Sound was played already, so toggle the sound off
                    g_smSndVolume.Remove(soundFile);
                    StopSoundAll(soundFile, entity, common);
                    DHookSetReturn(hReturn, false);
                    return MRES_Supercede;
                }
            }
            else
            {
                if(strcmp(eCommand, "PlaySound", false) == 0 || strcmp(eCommand, "ToggleSound", false) == 0)
                    g_smSndVolume.SetValue(soundFile, 10.0, true);
                else if(strcmp(eCommand, "Volume", false) == 0)
                    g_smSndVolume.SetValue(soundFile, fParam, true);
            }
        }

        if(g_smRecentSnd.GetValue(soundFile, temp))
        {
            g_smRecentSnd.Remove(soundFile);
            g_smSndCommon.SetValue(soundFile, 1, true);
            common = true;
            AddToStringTable(g_iSTSound, FakePrecacheSound(soundFile, true));
            PrecacheSound(FakePrecacheSound(soundFile, true), false);
        }
        else
        {
            AddToStringTable(g_iSTSound, FakePrecacheSound(soundFile, common));
            PrecacheSound(FakePrecacheSound(soundFile, common), false);
        }

        SendSound(soundFile, entity, common, fLength);

        if(!common && !(eFlags & 1))
        {
            g_smRecentSnd.SetValue(soundFile, 1, true);
            DataPack dataPack;
            CreateDataTimer(0.6, CheckCommonSounds, dataPack);
            dataPack.WriteString(soundFile);
            dataPack.WriteCell(entity);
        }
        DHookSetReturn(hReturn, false);
        return MRES_Supercede;
    }
    else if(strcmp(eCommand, "StopSound", false) == 0 || strcmp(eCommand, "FadeOut", false) == 0 || (strcmp(eCommand, "Volume", false) == 0 && (fParam < 0.1)))
    {
        int temp;
        bool common = g_smSndCommon.GetValue(soundFile, temp);
        StopSoundAll(soundFile, entity, common);

        if(eFlags & 1)
            g_smSndVolume.Remove(soundFile);

        return MRES_Ignored;
    }

    return MRES_Ignored;
}
/*
float GetSoundLengthFloat(const char[] soundFile) {
	char sDir[PLATFORM_MAX_PATH];
	FormatEx(sDir, sizeof(sDir), "sound/%s", soundFile);

	Handle hSoundFile = OpenSoundFile(sDir, true);
	if (hSoundFile == INVALID_HANDLE) return 0.0;

	float result = FloatMul(float(GetSoundLengthInMilliseconds(hSoundFile)), 0.001);
	delete hSoundFile;
	return result;
}
*/
static float GetSoundLengthFloat(const char[] file)
{
	float result = 0.0;

	if(g_smSndLength.GetValue(file, result))
		return result;

	char path[256];

	// in sound folder
	if(file[0] == '*' || file[0] == '#' || file[0] == '~' || file[0] == ')')
		FormatEx(path, 256, "sound/%s", file[1]);
	else
		FormatEx(path, 256, "sound/%s", file);

	// if(g_pSoundLib == 2)
	if(g_pSoundLib == 2 || g_pSoundLib == 1)
	{
		Handle sound = OpenSoundFile(path, true);
		if(sound == null)
		{
			LogMessage("Failed to open sound [%s] in [%s]", file, path);
			g_smSndLength.SetValue(file, result, true);
			return result;
		}
		result = GetSoundLengthInMilliseconds(sound) / 1000.0;
		g_smSndLength.SetValue(file, result, true);
		delete sound;
	}
/*
	else if(g_pSoundLib == 1)
	{
		Sound sound = new Sound(path, true);
		if(sound == null)
		{
			g_smSndLength.SetValue(file, result, true);
			return result;
		}
		result = sound.GetLength();
		g_smSndLength.SetValue(file, result, true);
		delete sound;
	}
*/
	return result;
}

public int GetSourceEntity(int entity)
{
    char seName[64];
    GetEntPropString(entity, Prop_Data, "m_sSourceEntName", seName, 64);
    if(seName[0])
    {
        int entRef;
        if(g_smSourceEnt.GetValue(seName, entRef))
        {
            int sourceEnt = EntRefToEntIndex(entRef);
            if(IsValidEntity(sourceEnt))
                return sourceEnt;
        }
    }
    return entity;
}

public Action CheckCommonSounds(Handle timer, DataPack dataPack)
{
    dataPack.Reset();
    char soundFile[256];
    dataPack.ReadString(soundFile, 256);
    g_smRecentSnd.Remove(soundFile);
    int temp;
    if(g_smSndCommon.GetValue(soundFile, temp))
    {
        temp = dataPack.ReadCell();
        StopSoundAll(soundFile, temp, false);
    }
    return Plugin_Stop;
}

public void OnEntityCreated(int entity, const char[] classname)
{
    if(!IsValidEdict(entity))
        return;

    if(classname[0] == 'a' && strcmp(classname, "ambient_generic") == 0)
    {
        DHookEntity(hAcceptInput, false, entity);
        SDKHook(entity, SDKHook_SpawnPost, OnEntitySpawned);
    }
    else if(classname[0] == 'p' && strcmp(classname, "point_clientcommand") == 0)
        DHookEntity(hAcceptInput, false, entity);
}

public void OnEntitySpawned(int entity)
{
    char seName[64], eName[64];
    GetEntPropString(entity, Prop_Data, "m_sSourceEntName", seName, 64);
    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");
    
   
    if(!(eFlags & 1) && seName[0])
    {
        int count = GetEntityCount();
        for(int i = 0; i <= count; i++)
            if(IsValidEntity(i))
            {
                GetEntPropString(i, Prop_Data, "m_iName", eName, 64);
                if(strcmp(seName, eName, false) == 0)
                {
                    g_smSourceEnt.SetValue(seName, EntIndexToEntRef(i), true);
                    return;
                }
            }
    }
}

static void SendSound(char[] name, int entity, bool common = false, float length, bool updatevol = false, int client = -1)
{
    if(!IsValidEntity(entity))
        return;

    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");

    if(eFlags & 1)
    {
        int customChannel;

        if(!g_smCtChannel.GetValue(name, customChannel))
        {
            g_smCtChannel.SetValue(name, g_iChannel, false);
            customChannel = g_iChannel;
            g_iChannel++;
            if(g_iChannel > SNDCHAN_USER_BASE)
                g_iChannel = SNDCHAN_USER_BASE - 75;
        }

        if(updatevol && IsClientInGame(client) && !g_bBGMply[client] && GetVolume(client) > 0)
        {
            EmitSoundToClient(client, FakePrecacheSound(name, common), client, customChannel, SNDLEVEL_NORMAL, SND_CHANGEVOL, g_fBGMVol[client], SNDPITCH_NORMAL, -1, _, _, true);
            return;
        }

        for(int i = 1; i <= MaxClients; i++)
            if(IsClientInGame(i) && !g_bBGMply[i] && GetVolume(i) > 0)
                EmitSoundToClient(i, FakePrecacheSound(name, common), i, customChannel, SNDLEVEL_NORMAL, SND_NOFLAGS, g_fBGMVol[i], SNDPITCH_NORMAL, -1, _, _, true);
    }
    else
    {
        int sourceEnt = GetSourceEntity(entity);

        if(updatevol && IsClientInGame(client) && !g_bBGMply[client] && GetVolume(client) > 0)
        {
            EmitSoundToClient(client, FakePrecacheSound(name, common), sourceEnt, SNDCHAN_USER_BASE, SNDLEVEL_NORMAL, SND_CHANGEVOL, g_fBGMVol[client], SNDPITCH_NORMAL, -1, _, _, true);
            return;
        }

        for(int i = 1; i <= MaxClients; i++)
            if(IsClientInGame(i) && !g_bBGMply[i] && GetVolume(i) > 0)
                EmitSoundToClient(i, FakePrecacheSound(name, common), sourceEnt, SNDCHAN_USER_BASE, SNDLEVEL_NORMAL, SND_NOFLAGS, g_fBGMVol[i], SNDPITCH_NORMAL, -1, _, _, true);
    }

    DataPack pack;
    CreateDataTimer(length, Timer_OnSoundEnd, pack, TIMER_FLAG_NO_MAPCHANGE);
    pack.WriteString(name);
    pack.WriteCell(g_iNumRound);
}

public Action Timer_OnSoundEnd(Handle timer, DataPack pack)
{
    pack.Reset();
    char soundFile[256];
    pack.ReadString(soundFile, 256);
    int iTimerRoundNum = pack.ReadCell();

    if(iTimerRoundNum == g_iNumRound)
        g_smSndVolume.Remove(soundFile);
}

static void ClientSendSound(char[] name, int client, bool common = false)
{
    if(!ClientIsValid(client))
        return;

    int customChannel;

    if(!g_smCtChannel.GetValue(name, customChannel))
    {
        g_smCtChannel.SetValue(name, g_iChannel, false);
        customChannel = g_iChannel;
        g_iChannel++;
        if(g_iChannel > SNDCHAN_USER_BASE)
            g_iChannel = SNDCHAN_USER_BASE - 75;
    }

    if(!g_bBGMply[client] && GetVolume(client) > 0)
        EmitSoundToClient(client, FakePrecacheSound(name, common), client, customChannel, SNDLEVEL_NORMAL, SND_NOFLAGS, g_fBGMVol[client], SNDPITCH_NORMAL, -1, _, _, true);
}

static void ClientStopSound(int client, const char[] name = "", bool common = false)
{
    if(name[0])
    {
        int customChannel;
        StopSound(client, (g_smCtChannel.GetValue(name, customChannel)) ? customChannel : SNDCHAN_USER_BASE, FakePrecacheSound(name, common));
    }
    else
    {
        ClientCommand(client, "playgamesound Music.StopAllExceptMusic");
        ClientCommand(client, "playgamesound Music.StopAllMusic");
    }
}

static void StopSoundAll(const char[] name, int entity, bool common = false)
{
    if(!IsValidEntity(entity))
        return;

    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");
    if(eFlags & 1)
    {
        for(int i = 1; i <= MaxClients; i++)
            if(IsClientInGame(i) && !g_bBGMply[i] && GetVolume(i) > 0)
                ClientStopSound(i, name, common);
    }
    else
    {
        int sourceEnt = GetSourceEntity(entity);
        StopSound(sourceEnt, SNDCHAN_USER_BASE, FakePrecacheSound(name, common));
    }
}

static void ClientUpdateSoundsVolume(int client)
{
    int entity = INVALID_ENT_REFERENCE;
    while((entity = FindEntityByClassname(entity, "ambient_generic")) != INVALID_ENT_REFERENCE)
    {
        if(GetHammerIdOfEntity(entity) <= 0)
            continue;

        char soundFile[256];
        GetEntPropString(entity, Prop_Data, "m_iszSound", soundFile, 256);

        float fVol = 0.0;
        if(!g_smSndVolume.GetValue(soundFile, fVol))
            continue;

        if(fVol < 0.1)
            continue;

        int temp;
        bool common = g_smSndCommon.GetValue(soundFile, temp);

        SendSound(soundFile, entity, common, 0.0, true, client);
    }
}

static char[] FakePrecacheSound(const char[] sample, const bool common = false)
{
    char szSound[256];
    strcopy(szSound, 256, sample);
    if(common)
    {
        if(szSound[0] != '*')
        {
            if(szSound[0] == '#')
                Format(szSound, 256, "*%s", szSound[1]);
            else
                Format(szSound, 256, "*%s", szSound);
        }
    }
    else
    {
        if(szSound[0] == '*' || szSound[0] == '#')
            Format(szSound, 256, "%s", szSound[1]);
    }
    return szSound;
}

static int GetHammerIdOfEntity(int entity)
{
    return IsValidEntity(entity) ? GetEntProp(entity, Prop_Data, "m_iHammerID") : -1;
}

public Action Hook_NormalSound(int clients[64], int &numClients, char sample[PLATFORM_MAX_PATH], int &entity, int &channel, float &volume, int &level, int &pitch, int &flags)
{
	// Ignore non-weapon sounds.
	if (!g_bHooked || !(strncmp(sample, "weapons", 7) == 0 || strncmp(sample[1], "weapons", 7) == 0))
		return Plugin_Continue;
	
	int i, j;
	
	for (i = 0; i < numClients; i++)
	{
		if (g_bWeapon[clients[i]])
		{
			// Remove the client from the array.
			for (j = i; j < numClients-1; j++)
			{
				clients[j] = clients[j+1];
			}
			
			numClients--;
			i--;
		}
	}
	
	return (numClients > 0) ? Plugin_Changed : Plugin_Stop;
}

public Action CSS_Hook_ShotgunShot(const char[] te_name, const Players[], int numClients, float delay)
{
	if (!g_bHooked)
		return Plugin_Continue;
	
	// Check which clients need to be excluded.
	int newClients[MAXPLAYERS];
	int client;
	int newTotal = 0;
	
	for (int i = 0; i < numClients; i++)
	{
		client = Players[i];
		
		if (!g_bWeapon[client])
		{
			newClients[newTotal++] = client;
		}
	}
	
	// No clients were excluded.
	if (newTotal == numClients)
		return Plugin_Continue;
	
	// All clients were excluded and there is no need to broadcast.
	else if (newTotal == 0)
		return Plugin_Stop;
	
	// Re-broadcast to clients that still need it.
	float vTemp[3];
	TE_Start("Shotgun Shot");
	TE_ReadVector("m_vecOrigin", vTemp);
	TE_WriteVector("m_vecOrigin", vTemp);
	TE_WriteFloat("m_vecAngles[0]", TE_ReadFloat("m_vecAngles[0]"));
	TE_WriteFloat("m_vecAngles[1]", TE_ReadFloat("m_vecAngles[1]"));
	TE_WriteNum("m_weapon", TE_ReadNum("m_weapon"));
	TE_WriteNum("m_iMode", TE_ReadNum("m_iMode"));
	TE_WriteNum("m_iSeed", TE_ReadNum("m_iSeed"));
	TE_WriteNum("m_iPlayer", TE_ReadNum("m_iPlayer"));
	TE_WriteFloat("m_fInaccuracy", TE_ReadFloat("m_fInaccuracy"));
	TE_WriteFloat("m_fSpread", TE_ReadFloat("m_fSpread"));
	TE_Send(newClients, newTotal, delay);
	
	return Plugin_Stop;
}

public Action DODS_Hook_FireBullets(const char[] te_name, const Players[], int numClients, float delay)
{
	if (!g_bHooked)
		return Plugin_Continue;
	
	// Check which clients need to be excluded.
	int newClients[MAXPLAYERS];
	int client;
	int newTotal = 0;
	
	for (int i = 0; i < numClients; i++)
	{
		client = Players[i];
		
		if (!g_bWeapon[client])
		{
			newClients[newTotal++] = client;
		}
	}
	
	// No clients were excluded.
	if (newTotal == numClients)
		return Plugin_Continue;
	
	// All clients were excluded and there is no need to broadcast.
	else if (newTotal == 0)
		return Plugin_Stop;
	
	// Re-broadcast to clients that still need it.
	float vTemp[3];
	TE_Start("FireBullets");
	TE_ReadVector("m_vecOrigin", vTemp);
	TE_WriteVector("m_vecOrigin", vTemp);
	TE_WriteFloat("m_vecAngles[0]", TE_ReadFloat("m_vecAngles[0]"));
	TE_WriteFloat("m_vecAngles[1]", TE_ReadFloat("m_vecAngles[1]"));
	TE_WriteNum("m_weapon", TE_ReadNum("m_weapon"));
	TE_WriteNum("m_iMode", TE_ReadNum("m_iMode"));
	TE_WriteNum("m_iSeed", TE_ReadNum("m_iSeed"));
	TE_WriteNum("m_iPlayer", TE_ReadNum("m_iPlayer"));
	TE_WriteFloat("m_flSpread", TE_ReadFloat("m_flSpread"));
	TE_Send(newClients, newTotal, delay);
	
	return Plugin_Stop;
}
