(* ::Package:: *)

BeginPackage["STTCalculator`"]



Begin["`Private`"]


arrangeVoyageSkillsForPredictor[{primary_String,secondary_String},skills_Association]:=PadRight[Join[{skills[primary],skills[secondary]},Values[KeyDrop[skills,{primary,secondary}]]],6]/.Missing[___]|0->{0,0}


voyageEstimate[{primary_String,secondary_String},crewskillranges_Association]:=
estimateVoyage[Join[{2500},arrangeVoyageSkillsForPredictor[{primary,secondary},crewskillranges]]]



crewminmaxskills[crewname_String]:=Module[{skillassoc,validskills,minmaxSkillValues},

	skillassoc=crewBonusSkills[crewname]/.Missing[___]->emptyskillassoc;

	minmaxSkillValues[skill_String]:={
	skillassoc[skill]["Base"]+skillassoc[skill]["Min Bonus"],
	skillassoc[skill]["Base"]+skillassoc[skill]["Max Bonus"]
	};

	validskills:=Select[skillassoc,#["Base"]>0&]//Keys;

	AssociationMap[minmaxSkillValues,validskills]
]

SetAttributes[crewminmaxskills,Listable];


buildVoyageCrew[skills_List,whichlist_List]:=Module[{sortByNewEstimate,estimateByAddingOneCrew,chosen,restoftheskills,orderedskills,skill,crewminmax,skillSortFunction,sortedcrewbyskill,inductee,chosenaggregateskills},
chosen=<|"Command"-><||>,"Diplomacy"-><||>,"Security"-><||>,"Engineering"-><||>,"Science"-><||>,"Medicine"-><||>|>;

restoftheskills:=Complement[primaryskills,skills];
crewminmax=Association[ParallelMap[#->crewminmaxskills[#]&,whichlist,DistributedContexts->All]];
orderedskills=Keys@Sort@Counts@Flatten@Keys@Values@Normal[crewminmax];(*sort by number of crew members with each skill*)

chosenaggregateskills:=Values@Values[chosen];
estimateByAddingOneCrew[aggregateskills_List,newcrewskills_Association]:=voyageEstimate[skills,Merge[{aggregateskills,newcrewskills},Total]];
sortByNewEstimate[crewminmax_Association,existingaggregateskills_List]:=KeyTake[crewminmax,Keys[ReverseSort[ParallelMap[estimateByAddingOneCrew[existingaggregateskills,#]&,crewminmax,DistributedContexts->All]]]];

(*seed the crew list*)
sortedcrewbyskill=crewminmax;

Do[
Scan[
Module[{skillinquestion=#},
(*sort the potential crew based on the current list of potentials*)
sortedcrewbyskill=sortByNewEstimate[sortedcrewbyskill,chosenaggregateskills];
(*take the crew that increases the estimate the most and has the skill we need*)
inductee=Take[Select[sortedcrewbyskill,FreeQ[#[skillinquestion],Missing|0|{0,0}]&],1];
(*add that crew to the list of chosen crew, assigned to the skill we need*)
AppendTo[chosen[skillinquestion],inductee];
(*drop the selected crew from the list of potential crew*)
KeyDropFrom[sortedcrewbyskill,Keys[inductee]];

]&
,orderedskills],
2];

chosen->voyageEstimate[skills,Merge[Values@Values[chosen],Total]]
]


buildVoyagePanel=FormPage[
FormObject[{"Skills"->CompoundElement[{"Primary"->primaryskills,
"Secondary"->primaryskills}],
"Crewoption"->{"All","Sans Shuttle Crew"},
"Traits"-><|"Interpreter"->RepeatingElement["String"],"Required"->False|>,
"Variants"-><|"Interpreter"->RepeatingElement["String"],"Required"->False|>,
"Shuttlebound"-><|"Interpreter"->RepeatingElement["String"],"Required"->False|>
}],
(fpresult="Processing";
SessionSubmit@Module[{myactivecrew45=myactivecrew[4,5],variants=#Variants,traits=#Traits,crewwithtraits={},crewwithvariants={},crewsetting=#Crewoption,sansshuttlecrew,voyageestimate,crewoption,shuttlecrew=#Shuttlebound},

If[crewsetting=="Sans Shuttle Crew",
crewwithtraits=Select[myactivecrew45,ContainsAny[crewstats[#]["Traits"],traits]&];
crewwithvariants=Select[myactivecrew45,ContainsAny[{crewstats[#]["CharName"]},variants]&];
sansshuttlecrew=Complement[myactivecrew45,shuttlecrew,crewwithtraits,crewwithvariants];
];


crewoption=Switch[crewsetting,"All",myactivecrew45,"Sans Shuttle Crew",sansshuttlecrew];

voyageestimate=buildVoyageCrew[{#Skills["Primary"],#Skills["Secondary"]},crewoption];

fpresult=Column[{Dataset[Keys[voyageestimate]][All,AssociationMap[crewstats[#]["Traits"]&,Keys[#]]&],Values[voyageestimate]}];

];
Dynamic[fpresult])&
]


(*customization for mixed list of all 4* combined with my 5* *)

build45VoyageCrew[skills_List]:=Module[{chosen,restoftheskills,orderedskills,skill,crewminmax,skillSortFunction,sortedcrewbyskill,inductee,crew4minmax,crew5minmax,crew4=stargroup[4],crew5=mycrew[5],sortByNewEstimate,estimateByAddingOneCrew,chosenaggregateskills},
chosen=<|"Command"-><||>,"Diplomacy"-><||>,"Security"-><||>,"Engineering"-><||>,"Science"-><||>,"Medicine"-><||>|>;

crew4minmax=Block[{$UseCurrentSkills=False},Association[ParallelMap[#->crewminmaxskills[#]&,crew4,DistributedContexts->All]]];
crew5minmax=Block[{$UseCurrentSkills=True},Association[ParallelMap[#->crewminmaxskills[#]&,crew5,DistributedContexts->All]]];
crewminmax=Join[crew4minmax,crew5minmax];

chosenaggregateskills:=Values@Values[chosen];
estimateByAddingOneCrew[aggregateskills_List,newcrewskills_Association]:=voyageEstimate[skills,Merge[{aggregateskills,newcrewskills},Total]];
sortByNewEstimate[crewminmax_Association,existingaggregateskills_List]:=KeyTake[crewminmax,Keys[ReverseSort[ParallelMap[estimateByAddingOneCrew[existingaggregateskills,#]&,crewminmax,DistributedContexts->All]]]];

(*seed the crew list*)
sortedcrewbyskill=crewminmax;

Do[
Scan[
Module[{skillinquestion=#},
(*sort the potential crew based on the current list of potentials*)
sortedcrewbyskill=sortByNewEstimate[sortedcrewbyskill,chosenaggregateskills];
(*take the crew that increases the estimate the most and has the skill we need*)
inductee=Take[Select[sortedcrewbyskill,FreeQ[#[skillinquestion],Missing|0|{0,0}]&],1];
(*add that crew to the list of chosen crew, assigned to the skill we need*)
AppendTo[chosen[skillinquestion],inductee];
(*drop the selected crew from the list of potential crew*)
KeyDropFrom[sortedcrewbyskill,Keys[inductee]];

]&
,orderedskills],
2];

chosen->voyageEstimate[skills,Merge[Values@Values[chosen],Total]]
]


End[]
EndPackage[]
