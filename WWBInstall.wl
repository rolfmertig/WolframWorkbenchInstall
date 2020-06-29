(* ::Package:: *)

(* ::Title:: *)
(*Automatic Wolfram Workbench installation and configuration*)


(* ::Abstract:: *)
(*Multi-OS automatic Installation and configuration (dark theme, and more) of  Eclipse 2020-06 and the  Wolfram Workbench (WWB) plugin, plus eventually other plugins*)


(* ::Author:: *)
(*Dr. Rolf Mertig,*)


(* ::Affiliation:: *)
(*GluonVision GmbH, Berlin, Germany  (http://www.gluonvision.com)*)


(* ::Text:: *)
(*Date: June 30th 2020*)


(* ::Item:: *)
(*Run this script from wolframscript or the Mathematica Kernel or the notebook Wolfram Mathematica Front End from WIndows, macOS or Linux*)


(* ::Item:: *)
(*Wolfram Mathematica (or Wolfram Engine) 12.1 is tested, Mathematica 12.0 should work*)


(* ::Item:: *)
(*Depending on your location please consider downloading this script first to your computer and change the value of EclipseMirror in $configuration*)


(* ::Item:: *)
(*You might also want to change the DownloadFolder or EclipseWorkspace settings*)


(* ::Text:: *)
(*Copyright (c) 2020 Dr. Rolf Mertig (rolfm@gluonvision.com)*)
(*This script is licenced under the MIT license ( https://en.wikipedia.org/wiki/MIT_License )*)
(*Further licenses relevant are https://www.wolfram.com/legal/terms/wolfram-engine.html and https://www.wolfram.com/legal/agreements/wolfram-mathematica/ and the license text mentioned in  https://support.wolfram.com/27221*)
(**)


(* ::Section:: *)
(*basic configuration*)


(* ::Subsection:: *)
(*$configuration specifications:  Eclipse 2020-06 version, default workspace eclipse-workspace-2020-06 in $HomeDirectory*)


$configuration = <|
  "EclipseMirror" -> "http://ftp.fau.de/eclipse/technology/epp/downloads/release/2020-06/R/"
  , "MacOSX"  -> <| "SourceFile" -> "eclipse-java-2020-06-R-macosx-cocoa-x86_64.dmg"  , "EclipseFolder" -> "/Applications/Eclipse.app" |>
  , "Windows" -> <| "SourceFile" -> "eclipse-java-2020-06-R-win32-x86_64.zip"         , "EclipseFolder" ->  FileNameJoin[{$HomeDirectory, "eclipse2020-06"}] |>
  , "Unix"    -> <| "SourceFile" -> "eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz"  , "EclipseFolder" ->  FileNameJoin[{$HomeDirectory, "eclipse2020-06"}] |>
  , "DownloadFolder" :> $TemporaryDirectory (* FileNameJoin[{$HomeDirectory, "Downloads"}] *) (* if downloads should be persistent, use this setting instead of $TemporaryDirectory *)
  , "EclipseWorkspace" :> FileNameJoin[{$HomeDirectory, "eclipse-workspace-2020-06"}]
|>;


(* ::Section:: *)
(*special settings (dark theme, colors for the WWB Wolfram Language editor *)


(* ::Subsection:: *)
(*wolframInst[] changes the default Wolfram Engine installation setting in the WWB plugin, before first use (don't touch this)*)


(* This fixes the standard "Wolfram Engine Installation to the one running this script, see also  https://reference.wolfram.com/workbench/index.jsp?topic=/com.wolfram.eclipse.help/html/reference/preferences/main.html *)
(* TODO: convince WRI to do this better ... *)
wolframInst[] := With[
  { instDir =
    If[$OperatingSystem === "MacOSX"
      , StringReplace[$InstallationDirectory, "Contents" -> ""]
      , If[$OperatingSystem === "Windows"
          , StringReplace[$InstallationDirectory, {"\\" -> "\\\\\\\\", ":" -> "\\\\\\:"}]
          , $InstallationDirectory
        ]
    ]
  }
  ,
  StringReplace[ByteArrayToString @
    ByteArray @ "d29sZnJhbS5tYXRoZW1hdGljYS5pbnN0YWxsYXRpb25zLmxpc3Q9XCNcclxuXCNEQVRFXHJcbmZlX3BhdGhcPU1NQUlOU1RESVJcXFxcTWF0aGVtYXRpY2EuZXhlXHJcbm92ZXJyaWRlX3N0YW5kYXJkX2V4ZWN1dGFibGVzXD1mYWxzZVxyXG5pbnN0YWxsX3Jvb3RcPU1NQUlOU1RESVJcclxubmFtZVw9TU1BVkVSU0lPTlxyXG5rZXJuZWxfcGF0aFw9TU1BSU5TVERJUlxcXFxNYXRoS2VybmVsLmV4ZVxyXG5cblxuXG5cbi0tLVxuXG5cblxu"
    ,
    {   "MMAINSTDIR" -> instDir
      , "MMAVERSION" -> First[StringSplit[$Version, " "]]
      , "DATE" -> StringReplace[ DateString[{"DayNameShort", " ", "MonthNameShort", " ", "Day", " ", "Hour", ":", "Minute", ":", "Second", " ", "TimeZoneName", " ", "Year"}], ":" -> "\\:"]
    }
  ]
];


(* ::Subsection:: *)
(*specific settings (like colors)*)


(* generate all default preferences files in the relevant folder created by createPrefsFiles *)
setDefaultPreferences[config_:$configuration] :=
      createPrefsFiles[ config @ "EclipseWorkspace" (* per workspace, as specificied by EclipseWorkspace -> ... *)
        (*  prefsFileName -> { "line1", "line2", ... } *)
        ,
        <|
          "com.wolfram.eclipse.MEET.prefs" ->
            {
                "com.wolfram.eclipse.editor.autocomplete.autoActivation=false"
              , "eclipse.preferences.version=1"
              , "editor.autocomplete.lowerCaseAndCamelCaseCompletion=true"
              , "editor.autocomplete.showOptions=true"
              , "editor.autocomplete.showOptions.optionValues=true"
              , "editor.default.sort.outline=true"
              , "syntaxcolor.comment=255,106,211"
              , "syntaxcolor.number.literal=221,221,221"
              , "syntaxcolor.private.non.system.symbol=210,210,210"
              , "syntaxcolor.public.non.system.symbols=182,253,85"
              , "syntaxcolor.string.literal=242,151,13"
              , "syntaxcolor.symbol.package=0,111,222"
              , "syntaxcolor.symbol.system=104,151,187"
              , "wolfram.mathematica.installations.default.installation.name=" <> StringTake[$Version, 6]
              , wolframInst[]
            }
          ,
          "org.eclipse.core.resources.prefs" ->
            {
                "eclipse.preferences.version=1"
              , "version=1"
            }
          ,
          "org.eclipse.e4.ui.css.swt.theme.prefs" ->
            {
                "eclipse.preferences.version=1"
              , "themeid=org.eclipse.e4.ui.css.theme.e4_dark"   (* dark theme *)
            }
          ,
          "org.eclipse.ui.ide.prefs" ->
            {
                "PROBLEMS_FILTERS_MIGRATE=true"
              , "SWITCH_PERSPECTIVE_ON_PROJECT_CREATION=always"
              , "TASKS_FILTERS_MIGRATE=true"
              , "eclipse.preferences.version=1"
              , "quickStart=false"
              , "tipsAndTricks=true"
            }
          ,
          "org.eclipse.ui.prefs" ->
            {
                "defaultPerspectiveId=com.wolfram.eclipse.MEET.mathematicaPerspective" (* start with Mathematica perspective *)
              , "eclipse.preferences.version=1"
              , "showIntro=false"
            }
          ,
          (* This changes the default autosave setting to True and the interval to 1 second. Change it as desired. *)
          "org.eclipse.ui.workbench.prefs" ->
            {
                "//org.eclipse.ui.commands/state/org.eclipse.ui.navigator.resources.nested.changeProjectPresentation/org.eclipse.ui.commands.radioState=false"
              , "PLUGINS_NOT_ACTIVATED_ON_STARTUP=;org.eclipse.m2e.discovery;"
              , "SAVE_AUTOMATICALLY=true"        (* enabling the "Autosave" checkbox in Preferences/General/Editors/Autosave *)
              , "SAVE_AUTOMATICALLY_INTERVAL=1"  (* setting the autosave minimal interval to 1 second *)
              , "eclipse.preferences.version=1"
              , "org.eclipse.ui.commands=<?xml version\\=\"1.0\" encoding\\=\"UTF-8\"?>\\r\\n<org.eclipse.ui.commands/>"
            }
        |>
];


(* ::Subsection:: *)
(*Additional plugins like vrapper (vim-plugin) and an IntelliJ KeyMap plugin*)


$AdditionalPlugins = {
  <| (* got Vim addicts *)
    "description" -> "Vrapper -- Vim-like editing in Eclipse"
    , "repository" -> "http://vrapper.sourceforge.net/update-site/unstable"
    , "installIU" -> "net.sourceforge.vrapper.feature.group"
  |>
  ,
  <| (* for users of IntelliJ *)
    "description" -> "IntelliJ KeyMap for Eclipse ( https://github.com/IntelliJIdeaKeymap4Eclipse/IntelliJIdeaKeymap4Eclipse )" , "repository" -> "https://dl.bintray.com/intellijideakeymap4eclipse/update-site"
    , "installIU" -> "com.github.intellijideakeymap4eclipse.feature.feature.group"
  |>
};


(* ::Subsection:: *)
(*The default setting is to only install the WWB pluigin*)


(* always install WWB *)
$plugins = {
  <|
    "description" -> "Wolfram Workbench Plugin for Eclipse"
    , "repository"  -> "http://workbench.wolfram.com/update"
    , "installIU"   -> "com.wolfram.eclipse.MEET.feature.group"
  |>
};


(* do not install those $AdditionalPlugins by default, since most people do not seem to use Vim, somehow, strangely, ... *)
If[!ValueQ[$InstallAdditionalPlugins], $InstallAdditionalPlugins = False];


If[TrueQ[$InstallAdditionalPlugins]
  ,
  (*i.e., if $InstallAdditionalPlugins=True is evaluated before running this script, then those plugins defined in $AdditionalPlugins are also installed *)
  $plugins = Join[$plugins, $AdditionalPlugins]
];


(* ::Section:: *)
(*Useful functions for preference files generation, script execution, logging and URLExistsQ *)


(* ::Subsubsection:: *)
(*createPrefsFiles*)


(* create the various .prefs file in the workspace/.metadata/.plugins/org.eclipse.core.runtime/.settings folder: *)
createPrefsFiles[workspaceLocation_String, prefsAsso_Association] := Module[{(*prefsDir*)}
  , "(* create prefs files in .metadata of the specificed workspace location *)"
  ; prefsDir = FileNameJoin[{workspaceLocation, ".metadata", ".plugins", "org.eclipse.core.runtime", ".settings"}]
  ; logPrint["Generating preferences in prefsDir = ", prefsDir]
  ; Do[If[ !TrueQ[DirectoryQ[prefsDir]], Pause[.2]; CreateDirectory[prefsDir] ], {5}]
  ; If[!DirectoryQ@prefsDir, logPrint["something went wrong when creating ", prefsDir," please try again. Exiting."]; Quit[]]
  ; KeyValueMap[Export[FileNameJoin[{prefsDir, #1}], #2, "Text"]&, prefsAsso ]
];


(* ::Subsubsection::Closed:: *)
(*logPrint, runCommand, bashRun*)


SetAttributes[logPrint, HoldAll];
logPrint[x__] := Echo[SequenceForm@@{x}, DateString[{"Hour", ":", "Minute", ":", "Second", ":", "Millisecond" }] <> "  "];

runCommand[ex_] := RunProcess[{$SystemShell, If[$OperatingSystem === "Windows",  "/c",  "-c"],  ex}, "ExitCode"];

bashRun[ex_String] := StringTrim @ RunProcess[{"/bin/bash", "-c", ex}, "StandardOutput"]; (* only needed in MacOSX ... *)

URLExistsQ[url_String] := JLink`JavaBlock @ Block[{openConnection, getResponseCode }
  , Needs["JLink`"]
  ; If[Head[JLink`JavaLink[]] =!= LinkObject, JLink`InstallJava[]]
  ; 200 === (JLink`JavaNew["java.net.URL", url] @ openConnection[] ) @ getResponseCode[]
];


(* ::Subsubsection:: *)
(*AdvancedURLDownload (* works only in the Front End ... yet ...*)*)


(* ::Text:: *)
(*Based on: https://mathematica.stackexchange.com/a/177283/29*)


(* A convenience function for downloading. Fall back to URLDOwnload if $Notebooks is False, otherwise print percentages on "stdout" *)
AdvancedURLDownload[file_, target_, label_String:""] := Module[
  {manifest, taskProgress, taskFinished, startJob, monitor}
  , If[!URLExistsQ[file], logPrint["The URL ", file," is not reachable. Quitting."]; Quit[]]
  ; If[$Notebooks =!= True, URLDownload[file, target]; Return[target, Module]]
  ; If[$OperatingSystem === "Windows", If[!MemberQ[$Echo, "stdout"], AppendTo[$Echo, "stdout"]] ] (* for visualizing WriteString["stdout","."] *)
  ; manifest = <||>
  ; SetAttributes[taskProgress, HoldFirst]
  ; taskProgress[manifest_][event_] := manifest = <|manifest, event["Task"] -> event|>
  ; SetAttributes[taskFinished, HoldFirst]
  ; taskFinished[manifest_][event_] := manifest = <|manifest, event["Task"] -> event|>
  ; SetAttributes[startJob, HoldFirst]
  ; startJob[manifest_][src_, dest_] := (
      monitor[manifest]
    ; URLDownloadSubmit[ src, dest
        , HandlerFunctions -> <|
            "TaskFinished" -> taskFinished[manifest] , "TaskProgress" -> taskProgress[manifest]
          |>
        , HandlerFunctionsKeys ->
            { "Task", "TaskStatus", "File", "ByteCountTotal", "ByteCountDownloaded", "FractionComplete" }
      ]
    )
  ; SetAttributes[monitor, HoldFirst]
  ; monitor[manifest_]:= Module[{m = True, p, percPrint},
      percPrint = {.1 -> ".[10%].", .25 -> ".[25%].", .5 -> ".[50%].", .75 -> ".[75%].", 1. -> ".[100%]"}
      ; If[MatchQ[Values[manifest],{__Association}]
          ,
            While[m ,
              p = First[Values[manifest]]["FractionComplete"]
              ; If[p == 1, m=False]
              ; If[ p >= percPrint[[1,1]]
                  ,
                    WriteString["stdout", percPrint[[1,2]] ]
                    ; If[Length[percPrint]>0, percPrint = Rest[percPrint]]
                ]
              ; Pause[.05]
            ]
            ; WriteString["stdout", " downloaded ", Round[First[Values[manifest]]["ByteCountDownloaded"]/1024^2], " MB"]
        ]
   ]
  ; WriteString["stdout", label <> " "]
  ; startJob[manifest][file, target]
  ; Do[ While[manifest === <||>, Pause[.1]], {50}]
  ; monitor[manifest]
];


(* ::Subsection:: *)
(*Download and install Eclipse automatically according to $configuration settings*)


(* ::Text:: *)
(*Install Eclipse according to settings in config. *)


EclipseInstall[config_Association:$configuration]:= Module[
  {eclipseDir, eclipseURL, dFile, mountPoint, tmp, tmpDir, eDir}

  , "(* multi-OS installation of Eclipse *)"
  ; eclipseDir = config[$OperatingSystem, "EclipseFolder"]

  ; If[DirectoryQ[eclipseDir]
      , logPrint["The folder ", eclipseDir, " is not empty, skipping Eclipse download and installation."]
      ; Return[eclipseDir, Module]
    ]

  ; eclipseURL = URLBuild[{config @ "EclipseMirror", config[$OperatingSystem, "SourceFile"]}]
  ; If[!URLExistsQ[eclipseURL], logPrint[eclipseURL," does not exists. Quitting."]; Quit[]]
  ; logPrint["Installing ", FileBaseName @ eclipseURL, " in ", eclipseDir ]

  ; dFile = FileNameJoin[{config["DownloadFolder"], FileNameTake[eclipseURL, -1]}]
  ; If[FileExistsQ[dFile], If[FileByteCount[dFile] < 10^8, DeleteFile @ dFile]]
  ; If[!FileExistsQ[dFile], AdvancedURLDownload[eclipseURL, dFile, "Downloading Eclipse:"]]
  ; If[$OperatingSystem === "MacOSX"
      ,
        mountPoint = StringTrim @ Last @ StringSplit[#, "\t"]& @ bashRun["hdiutil attach " <> dFile <> " -nobrowse | grep /Volumes/Eclipse"]
        ;	tmp = FileNames["*.app", mountPoint] /. {a_String} :> a
      ,
        ExtractArchive[dFile, tmpDir = CreateDirectory[]] (* this creates an eclipse folder in the directory created by CreateDirectory *)
        ; tmp = FileNames["eclipse", tmpDir] /. {d_String} :> d
    ]
  ; If[!DirectoryQ[tmp], logPrint["creating ",tmp," failed!"]; Return[$Failed, Module]]
  ; eDir = RenameDirectory[tmp, eclipseDir]
  ; If[eDir === $Failed, CopyDirectory[tmp, eclipseDir]]
  ; If[DirectoryQ[eclipseDir], logPrint["Installed Eclipse into ", eclipseDir]; eclipseDir,$Failed]
];



(* ::Section:: *)
(*WolframWorkbenchInstall function*)


WolframWorkbenchInstall[config_Association, startAfterInstallation_:True] := Module[
  {
    timeStart, downloadFolder, eclipseDir, eclipseSource, eclipseExe,  eclipseIni, downloadFile, iniFile
  }
    , "(* get folders from config *)"
    ; timeStart = AbsoluteTime[];
    ; {downloadFolder, eclipseDir} =  {config @ "DownloadFolder", config[$OperatingSystem, "EclipseFolder"]} (* eclipse installation folder *)
      (* Download Eclipse *)
    ; EclipseInstall[config]
    ; eclipseIni = FileNames["eclipse.ini", eclipseDir, Infinity] /. {e_String} :> e
    ; modifyIniFile[eclipseIni, config] (* specificy Eclipse to use the Java installation from the Wolfram Mathematica/Wolfram Engine installation*)
    ; logPrint["Modified ", eclipseIni]
    ; setDefaultPreferences[config]
    ; installPlugin[#, eclipseDir]& /@ $plugins
    ; If[TrueQ[startAfterInstallation]
         , (* use Run instead of SystemOpen, since it works from wolframscript *)
         logPrint["Launching Eclipse with the specified workspace, starting the Wolfram Workbench perspective"]
         ; If[MemberQ[{"Unix", "MacOSX"}, $OperatingSystem]
             ,
               eclipseExe = (getEclipseExe @ config[$OperatingSystem, "EclipseFolder"])
               ; If[FileExistsQ[eclipseExe], runCommand @ (eclipseExe <> "&")]
             ,
               Run[ "start " <> getEclipseExe @ config[$OperatingSystem, "EclipseFolder"] ]
           ]
    ]
    ; logPrint["Seconds needed to install WWB: ", Round[ AbsoluteTime[] - timeStart ] ]
      (* if run from a terminal: quit directly in order to avoid strange error messages on MacOSX ... *)
    ; If[$Notebooks =!= True, logPrint["Quitting now "]; Quit[]]
];

(* need eclipsec.exe on Windows *)
getEclipsecExe[eclipseDir_String]:=
  FileNameJoin[
    { eclipseDir,
        Switch[ $OperatingSystem,
          "Windows", "eclipsec.exe" (* https://wiki.eclipse.org/FAQ_How_do_I_run_Eclipse%3F *)
          , "Unix", "eclipse"
          , "MacOSX", FileNameJoin[{"Contents", "MacOS", "eclipse"}]
        ]
    }
];

(* the GUI eclipse executable *)
getEclipseExe[eclipseDir_String]:=
  StringReplace[getEclipsecExe[eclipseDir], "eclipsec.exe" -> "eclipse.exe"];

(* Make a copy of eclipse.ini (to eclipse.ini.orig) and modify eclipse.ini to use the Wolfram jvm and the predefined workspace from $configuration. *)
modifyIniFile[iniFile_String?FileExistsQ, config_:$configuration] :=  Module[{javaDir, pathnameSeparatorFix}
  ,
  "(* modification of the eclipse.ini file may have to be changed in future Eclipse version *)"
  ; If[!FileExistsQ[iniFile<>".orig"], CopyFile[iniFile, iniFile <> ".orig"]]
  ;  pathnameSeparatorFix = Function[dir, If[$OperatingSystem === "Windows", StringReplace[dir, $PathnameSeparator -> "/"], dir] ]
  (* Since there is already a full OpenJDK version present, use the Java installation of Mathematica, unless another vm is already specified: *)
  ; If[ And @@ StringFreeQ[Map[StringTrim]@Import[iniFile, "Lines"], "-vm"~~EndOfString] (* if no -vm  setting is already there *)
    ,
      javaDir = pathnameSeparatorFix @
      (*https://stackoverflow.com/a/316535*)
      Switch[$OperatingSystem
        , "Windows"
        , FileNameJoin[{$InstallationDirectory, "SystemFiles", "Java", $SystemID, "bin", "server", "jvm.dll"}]
        , "MacOSX"
        , FileNameJoin[{$InstallationDirectory, "SystemFiles", "Java", $SystemID, "lib", "server", "libjvm.dylib"}]
        , "Unix"
        , FileNameJoin[{$InstallationDirectory, "SystemFiles", "Java", $SystemID, "lib", "server", "libjvm.so"}]
     ]
    ; Export[iniFile, Join[
        {
           "-vm"
           , javaDir
           , "-data"
           , pathnameSeparatorFix @ config @ "EclipseWorkspace"
        }
        ,
           Import[iniFile,"Lines"]
        ]
        ,  "Lines"
      ]
    ]
];


(* ::Subsection:: *)
(*installPlugin*)


installPlugin[asso_Association,  eclipseDir_String] := Module[{bundlesInfo, instIU, makeOption, runExitCode, eclipseDirCmd},
   (* check if the plugin is already installed by investigating bundles.info *)
    bundlesInfo = FileNames["bundles.info", FileNameJoin[{eclipseDir, "configuration", "org.eclipse.equinox.simpleconfigurator"}]]/.{f_String} :> f
  ; instIU = StringReplace[asso["installIU"], ".feature.group" -> ""]
  ; If[ FindList[bundlesInfo, instIU] =!= {}
      ,
        logPrint["plugin already installed"]
        ; Return[True, Module]
    ]
  ; makeOption = (" -"<> # <> " " <> asso[#])&
  ; logPrint["Installing ", asso @ "description", " from ", asso @ "repository"]
  ; eclipseDirCmd = getEclipsecExe[eclipseDir] <>
      " -noSplash -clean -consolelog -application org.eclipse.equinox.p2.director" <>
      makeOption["repository"] <>
      makeOption["installIU"] <>
      " -destination " <> eclipseDir
  ; runExitCode = runCommand @ eclipseDirCmd
  ; If[runExitCode =!= 0, Return[$Failed, Module]]
  ; logPrint@"Plugin successfully installed"
];



(* ::Section:: *)
(*Run the installer*)


WolframWorkbenchInstall[$configuration, True (* start after installation *)]
