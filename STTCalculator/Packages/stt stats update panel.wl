(* ::Package:: *)

BeginPackage["STTCalculator`"]



Begin["`Private`"]

newcrewfield:=Column[{"Crew Name",InputField[Dynamic[selected],String,ContinuousAction->False,FieldCompletionFunction->(completionFunction[#,10]&)]}]

selectskills[crewname_String]:=DynamicModule[{crewstats},
	crewstats=PersistentValue["stt/crewstats"];
	selectedcrewstats=crewstats[crewname];
	selectedcrewskilldata=crewskills[crewname];
	selectedname=crewname;
	selectedchar=selectedcrewstats["CharName"];
	selectedstars=selectedcrewstats["Stars"];
	selectedrace=selectedcrewstats["Race"];
	selectedtraits=StringRiffle[selectedcrewstats["Traits"],", "];
	selectedstatus=crewstatus[selectedname];
	selectedcmd=selectedcrewskilldata["Command"];
		cmdbase=getskillvalue[selectedcmd,"Base"];
		cmdmin=getskillvalue[selectedcmd,"Min Bonus"];
		cmdmax=getskillvalue[selectedcmd,"Max Bonus"];
	selecteddip=selectedcrewskilldata["Diplomacy"];
		dipbase=getskillvalue[selecteddip,"Base"];
		dipmin=getskillvalue[selecteddip,"Min Bonus"];
		dipmax=getskillvalue[selecteddip,"Max Bonus"];
	selectedeng=selectedcrewskilldata["Engineering"];
		engbase=getskillvalue[selectedeng,"Base"];
		engmin=getskillvalue[selectedeng,"Min Bonus"];
		engmax=getskillvalue[selectedeng,"Max Bonus"];
	selectedsec=selectedcrewskilldata["Security"];
		secbase=getskillvalue[selectedsec,"Base"];
		secmin=getskillvalue[selectedsec,"Min Bonus"];
		secmax=getskillvalue[selectedsec,"Max Bonus"];
	selectedmed=selectedcrewskilldata["Medicine"];
		medbase=getskillvalue[selectedmed,"Base"];
		medmin=getskillvalue[selectedmed,"Min Bonus"];
		medmax=getskillvalue[selectedmed,"Max Bonus"];
	selectedsci=selectedcrewskilldata["Science"];
		scibase=getskillvalue[selectedsci,"Base"];
		scimin=getskillvalue[selectedsci,"Min Bonus"];
		scimax=getskillvalue[selectedsci,"Max Bonus"];
];

(*insert code for the 'processedcrew1'?*)
selectnewcrewstats[crew_Association]:=DynamicModule[{crewstats},
	crewstats=PersistentValue["stt/crewstats"];
	selectedcrewstats=crewstats[crewname];
	selectedcrewskilldata=crewskills[crewname];
	selectedname=crewname;
	selectedchar=selectedcrewstats["CharName"];
	selectedstars=selectedcrewstats["Stars"];
	selectedrace=selectedcrewstats["Race"];
	selectedtraits=selectedcrewstats["Traits"];
	selectedstatus=crewstatus[selectedname];
	selectedcmd=selectedcrewskilldata["Command"];
		cmdbase=getskillvalue[selectedcmd,"Base"];
		cmdmin=getskillvalue[selectedcmd,"Min Bonus"];
		cmdmax=getskillvalue[selectedcmd,"Max Bonus"];
	selecteddip=selectedcrewskilldata["Diplomacy"];
		dipbase=getskillvalue[selecteddip,"Base"];
		dipmin=getskillvalue[selecteddip,"Min Bonus"];
		dipmax=getskillvalue[selecteddip,"Max Bonus"];
	selectedeng=selectedcrewskilldata["Engineering"];
		engbase=getskillvalue[selectedeng,"Base"];
		engmin=getskillvalue[selectedeng,"Min Bonus"];
		engmax=getskillvalue[selectedeng,"Max Bonus"];
	selectedsec=selectedcrewskilldata["Security"];
		secbase=getskillvalue[selectedsec,"Base"];
		secmin=getskillvalue[selectedsec,"Min Bonus"];
		secmax=getskillvalue[selectedsec,"Max Bonus"];
	selectedmed=selectedcrewskilldata["Medicine"];
		medbase=getskillvalue[selectedmed,"Base"];
		medmin=getskillvalue[selectedmed,"Min Bonus"];
		medmax=getskillvalue[selectedmed,"Max Bonus"];
	selectedsci=selectedcrewskilldata["Science"];
		scibase=getskillvalue[selectedsci,"Base"];
		scimin=getskillvalue[selectedsci,"Min Bonus"];
		scimax=getskillvalue[selectedsci,"Max Bonus"];
]

saveButton:=Button["Save new stats",Module[
{},
crewstats=PersistentValue["stt/crewstats"];
crewstatus=PersistentValue["stt/crewstatus"];
crewmaxskills=PersistentValue["stt/crewmaxskills"];
crewcurrentskills=PersistentValue["stt/crewcurrentskills"];

AppendTo[crewstats,
selectedname-><|
"CharName"->selectedchar,
"Stars"->selectedstars,
"Race"->selectedrace,
"Traits"->Interpreter[DelimitedSequence["String",","]][selectedtraits]
|>
];

AppendTo[crewstatus,
selectedname->selectedstatus
];

If[selectedstatus==="Leveling",
AppendTo[crewcurrentskills,
selectedname-><|
"Command"-><|"Base"->cmdbase,"Min Bonus"->cmdmin,"Max Bonus"->cmdmax|>,
"Diplomacy"-><|"Base"->dipbase,"Min Bonus"->dipmin,"Max Bonus"->dipmax|>,
"Engineering"-><|"Base"->engbase,"Min Bonus"->engmin,"Max Bonus"->engmax|>,
"Security"-><|"Base"->secbase,"Min Bonus"->secmin,"Max Bonus"->secmax|>,
"Medicine"-><|"Base"->medbase,"Min Bonus"->medmin,"Max Bonus"->medmax|>,
"Science"-><|"Base"->scibase,"Min Bonus"->scimin,"Max Bonus"->scimax|>|>
];
crewupdatemessage="Updated current skills for "<>selectedname;
,
AppendTo[crewmaxskills,
selectedname-><|
"Command"-><|"Base"->cmdbase,"Min Bonus"->cmdmin,"Max Bonus"->cmdmax|>,
"Diplomacy"-><|"Base"->dipbase,"Min Bonus"->dipmin,"Max Bonus"->dipmax|>,
"Engineering"-><|"Base"->engbase,"Min Bonus"->engmin,"Max Bonus"->engmax|>,
"Security"-><|"Base"->secbase,"Min Bonus"->secmin,"Max Bonus"->secmax|>,
"Medicine"-><|"Base"->medbase,"Min Bonus"->medmin,"Max Bonus"->medmax|>,
"Science"-><|"Base"->scibase,"Min Bonus"->scimin,"Max Bonus"->scimax|>|>
];
crewupdatemessage="Updated max skills for "<>selectedname;
]
;

PersistentValue["stt/crewstats"]=crewstats;
PersistentValue["stt/crewstatus"]=crewstatus;
PersistentValue["stt/crewmaxskills"]=crewmaxskills;
PersistentValue["stt/crewcurrentskills"]=crewcurrentskills;
]]

crewUpdatePanel:=DynamicModule[{selected,targetskillassoc,crewupdatemessage},

selected=First@crewnames;
selectskills[selected];
targetskillassoc:=Switch[selectedstatus,
	"Leveling",
	crewcurrentskills,
	__,
	crewmaxskills
];

crewupdatemessage="Status goes here";

Panel[Grid[{
{newcrewfield,Button["Load",selectskills[selected]],saveButton},
{"Name: ",InputField[Dynamic[selectedname],String]},
{"Character: ",InputField[Dynamic[selectedchar],String]},
{"Stars: ",InputField[Dynamic[selectedstars],Number]},
{"Race: ",InputField[Dynamic[selectedrace],String]},
{"Traits: ",InputField[Dynamic[selectedtraits]]},
{"Status: ",InputField[Dynamic[selectedstatus],String]},
{"Stat","Base","Min Bonus","Max Bonus"},
{"Command:",InputField[Dynamic[cmdbase],Number], InputField[Dynamic[cmdmin],Number],InputField[Dynamic[cmdmax],Number]},
{"Diplomacy:",InputField[Dynamic[dipbase],Number], InputField[Dynamic[dipmin],Number],InputField[Dynamic[dipmax],Number]},
{"Engineering:",InputField[Dynamic[engbase],Number], InputField[Dynamic[engmin],Number],InputField[Dynamic[engmax],Number]},
{"Security:",InputField[Dynamic[secbase],Number], InputField[Dynamic[secmin],Number],InputField[Dynamic[secmax],Number]},
{"Science:",InputField[Dynamic[scibase],Number], InputField[Dynamic[scimin],Number],InputField[Dynamic[scimax],Number]},
{"Medicine:",InputField[Dynamic[medbase],Number], InputField[Dynamic[medmin],Number],InputField[Dynamic[medmax],Number]},
{Dynamic[crewupdatemessage],Dynamic[Hyperlink[StringJoin["https://stt.wiki/wiki/",URLEncode[StringReplace[selectedname," "->"_"]]]]],Button["Load new crew",selectskills[First@newcrew]]}
}]],
Initialization:>(
crewstats=PersistentValue["stt/crewstats"];
crewstatus=PersistentValue["stt/crewstatus"];
crewmaxskills=PersistentValue["stt/crewmaxskills"];
crewcurrentskills=PersistentValue["stt/crewcurrentskills"];
)
];


End[]

EndPackage[]




