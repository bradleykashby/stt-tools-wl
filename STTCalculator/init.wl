(* ::Package:: *)

BeginPackage["STTCalculator`"]

(*declare public symbols*)

selectBestCrew
sttTabbedPanel
estimateVoyage
crewUpdatePanel::usage = 
	"A panel to add or update crew stats and skills."
crewStatusPanel::usage = 
	"A panel for updating the status of your crew"
buildVoyagePanel
buildVoyageCrew
skillpairs
topInShuttleSkills
crewstatus
setNewStatus
crewKeeperPanel
		
EndPackage[]

Get/@FileNames["*.wl",DirectoryName[$InputFileName]<>"Packages/"];
