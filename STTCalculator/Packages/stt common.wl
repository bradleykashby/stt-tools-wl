(* ::Package:: *)

BeginPackage["STTCalculator`"]



Begin["`Private`"]


(*References to persistent values*)

crewstats=PersistentValue["stt/crewstats"];

crewstatus=PersistentValue["stt/crewstatus"];

crewmaxskills=PersistentValue["stt/crewmaxskills"];

crewcurrentskills=PersistentValue["stt/crewcurrentskills"];

crewnames:=Keys[PersistentValue["stt/crewstats"]];

crewskillbonuses:=PersistentValue["stt/crewskillbonuses"];

totalskillbonuses:=Merge[Values[PersistentValue["stt/crewskillbonuses"]],Merge[#,Total]&]

(*deleteSTTcrew[crew_String]:=Scan[KeyDropFrom[#,crew]&,{
PersistentValue["stt/crewstats"],
PersistentValue["stt/crewstatus"],
PersistentValue["stt/crewmaxskills"],
PersistentValue["stt/crewcurrentskills"]}]*)


(*groups of crew (stars, mycrew, active crew) and skill lists*)

stargroups:=Keys/@GroupBy[crewstats,#["Stars"]&];

stargroup[x__Integer]:=Flatten[Lookup[stargroups,{x}]];

stargroup[All]:=stargroup[1,2,3,4,5];

mycrew[Optional[stars__Integer,All]]:=Select[stargroup[stars],
ContainsAny[
{crewstatus[#]},
{"Active","Frozen","Leveling"}]&
];
mycrew[All]:=mycrew[];

myactivecrew[Optional[stars__Integer,All]]:=Select[stargroup[stars],
ContainsAny[
{crewstatus[#]},
{"Active","Leveling"}]&
];
myactivecrew[All]:=myactivecrew[];

primaryskills={"Command","Diplomacy","Engineering","Security","Medicine","Science"};

skillpairs=DeleteCases[Tuples[primaryskills,2],{x_,x_}];(*order matters for voyages*)

allskillcombos=DeleteDuplicates[Sort/@Subsets[primaryskills,{1,2}]];

shuttleskillcombos={{"Command"},{"Diplomacy"},{"Engineering"},{"Security"},{"Medicine"},{"Science"},{"Command","Diplomacy"},{"Command","Engineering"},{"Command","Security"},{"Diplomacy","Engineering"},{"Diplomacy","Science"},{"Diplomacy","Security"},{"Engineering","Science"},{"Engineering","Security"},{"Science","Security"}};

emptyskillassoc=<|
"Command"-><|"Base"->0,"Min Bonus"->0,"Max Bonus"->0|>,
"Diplomacy"-><|"Base"->0,"Min Bonus"->0,"Max Bonus"->0|>,
"Engineering"-><|"Base"->0,"Min Bonus"->0,"Max Bonus"->0|>,
"Security"-><|"Base"->0,"Min Bonus"->0,"Max Bonus"->0|>,"
Medicine"-><|"Base"->0,"Min Bonus"->0,"Max Bonus"->0|>,
"Science"-><|"Base"->0,"Min Bonus"->0,"Max Bonus"->0|>|>;


(*Retrieving skills for crew*)

$UseCurrentSkills=True;

$UseSkillBonuses=True;

(*get a sub-value of a skil's assoc, replacing missings*)
getskillvalue[skillassoc_Association,val_String]:=skillassoc[val]/.Missing[___]->0
getskillvalue[_Missing,___]:=0

Options[crewskills]={"Leveling"->True};

crewskills[crewname_String]:=Module[{},
If[$UseCurrentSkills,
	
	Switch[crewstatus[crewname],
	"Leveling",
		crewcurrentskills[crewname],
	__,
		crewmaxskills[crewname]],
	(*else*)
	crewmaxskills[crewname]
]/.Missing[___]->emptyskillassoc
];

(*crewBonusSkills does not yet recognize/respect $UseSkillBonuses, assumed to be True*)
(*use crewskills for a string, also take the skill assoc as input*)
crewBonusSkills[crew_String]:=Module[{totalskillbonuses=totalskillbonuses,skillassoc=crewskills[crew],mergingFunction,fullassoc},

mergingFunction[list_List]:=Module[{assoc},
assoc=Join@@list;

<|"Base"->Round[getskillvalue[assoc,"Base"]*Quantity[100+assoc["Core Skill"],"Percent"]],
"Min Bonus"->Round[getskillvalue[assoc,"Min Bonus"]*Quantity[100+assoc["Skill Proficiency"],"Percent"]],
"Max Bonus"->Round[getskillvalue[assoc,"Max Bonus"]*Quantity[100+assoc["Skill Proficiency"],"Percent"]]
|>
];

Merge[{totalskillbonuses,skillassoc},mergingFunction]

]

crewBonusSkills[skillassoc_Association]:=Module[{totalskillbonuses=totalskillbonuses,mergingFunction,fullassoc},

mergingFunction[list_List]:=Module[{assoc},
assoc=Join@@list;

<|"Base"->Round[getskillvalue[assoc,"Base"]*Quantity[100+assoc["Core Skill"],"Percent"]],
"Min Bonus"->Round[getskillvalue[assoc,"Min Bonus"]*Quantity[100+assoc["Skill Proficiency"],"Percent"]],
"Max Bonus"->Round[getskillvalue[assoc,"Max Bonus"]*Quantity[100+assoc["Skill Proficiency"],"Percent"]]
|>
];

Merge[{totalskillbonuses,skillassoc},mergingFunction]

]
SetAttributes[crewBonusSkills,Listable];

skillAvg[crewname_String]:=N[#["Base"]+Mean[{#["Min Bonus"],#["Max Bonus"]}]]&/@crewBonusSkills[crewname];
SetAttributes[skillAvg,Listable];

skillSum[crewname_String]:=Total[skillAvg[crewname]];
SetAttributes[skillSum,Listable];



(*other common helper functions*)

getCrewStatus[crewlist_List]:=KeyTake[crewstatus,crewlist];

(*completionFunction[name_,num_]:=Take[Autocomplete[crewnames,name,num],UpTo[num]];*)
completionFunction[name_,num_]:=Take[Select[crewnames,StringContainsQ[#,name,IgnoreCase->True]&],UpTo[15]]


(*tabbed panel*)

sttTabbedPanel:=TabView[{
(*"Crew Picker"\[Rule]crewKeeperPanel,*)
"Status"->crewStatusPanel,
"Stats Update"->crewUpdatePanel,
"Voyages"->buildVoyagePanel
}]


End[];

EndPackage[]

