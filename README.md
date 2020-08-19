# WolframWorkbenchInstall
Executing 
```wolframscript -code '$InstallAdditionalPlugins=True; Import["https://wolfr.am/NHGLnZMa"]'``` from Wolfram Engine or 
```$InstallAdditionalPlugins=True; Import["https://wolfr.am/NHGLnZMa"]``` from a Mathematica Notebook 
automatically downloads Eclipse2020-06, installs it to a folder Eclipse2020-06 (.app on macOS) 
and downloads, installs and configures Wolfram Workbench, an IDE plugin developed by Wolfram Research.
Omitting `$InstallAdditionalPlugins=True;` does not install the additional plugins listed in `$AdditionalPlugs`.

What happens exactly is commented in the code.