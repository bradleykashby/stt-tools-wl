(* ::Package:: *)

BeginPackage["STTCalculator`"]



Begin["`Private`"]

crewnamefield:=Column[{"Crew Name",InputField[Dynamic[statusinput],String,ContinuousAction->False,FieldCompletionFunction->(completionFunction[#,10]&)]}]

statusselectbar:=Column[{"Crew Status",RadioButtonBar[Dynamic[newstatus],{"Active","Leveling","Frozen","None"}]}]

statusdisplay:=Dynamic[Dataset[<|"Character"->#,"Stars"->crewstats[#]["Stars"],"Status"->crewstatus[#]|>&/@completionFunction[statusinput,10]],TrackedSymbols:>{statusinput}]

setNewStatus[crew_String,status_String]:=Module[{},
If[First[completionFunction[crew]]==crew,
crewstatus=PersistentValue["stt/crewstatus"];
crewstatus[crew]=status;
PersistentValue["stt/crewstatus"]=crewstatus;
message=crew<>" is now "<>status;,
message="There is no crew by the name "<>crew<>".";
]
]

buttons:=Column[{Button["Set New Status",setNewStatus[statusinput,newstatus]]}]

crewStatusPanel:=DynamicModule[{statusinput="",newstatus},
message="";
Panel[
Grid[{{Item[crewnamefield,Alignment->Top],Item[statusdisplay,ItemSize->{45,30},Alignment->Top]},
{statusselectbar,buttons},
{Dynamic[message]}}]
]
];


End[]

EndPackage[]
