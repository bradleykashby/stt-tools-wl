(* ::Package:: *)

BeginPackage["STTCalculator`"]




Begin["`Private`"]

skillShuttleTotal[crewskillassoc_Association,{skill1_String,skill2_String}]:=Max[crewskillassoc[skill1]["Base"],crewskillassoc[skill2]["Base"]]+Min[crewskillassoc[skill1]["Base"],crewskillassoc[skill2]["Base"]]/4//N;
skillShuttleTotal[crewskillassoc_Association,{skill1_String}]:=crewskillassoc[skill1]["Base"];

topInSkill[{skill1_String,skill2_String},whichlist_List,howmany_Integer:4]:=
Module[{thiscrewsskills,crewwithskills,shuttleskilltotals},

thiscrewsskills=AssociationMap[crewBonusSkills,whichlist];
crewwithskills=Select[thiscrewsskills,And[#[skill1]["Base"]>0,#[skill2]["Base"]>0]&];
shuttleskilltotals=skillShuttleTotal[#,{skill1,skill2}]&/@crewwithskills;

Take[ReverseSort[shuttleskilltotals],UpTo[howmany]]
];

topInSkill[{skill1_String},whichlist_,howmany_Integer:4]:=
Module[{thiscrewsskills,crewwithskills,shuttleskilltotals},
thiscrewsskills=AssociationMap[crewBonusSkills,whichlist];
crewwithskills=Select[thiscrewsskills,#[skill1]["Base"]>0&];
shuttleskilltotals=skillShuttleTotal[#,{skill1}]&/@crewwithskills;
Take[ReverseSort[shuttleskilltotals],UpTo[howmany]]
];

topInShuttleSkills[whichlist_List,howmany_Integer:4]:=Union[Flatten[Keys[topInSkill[#,whichlist,howmany]]&/@shuttleskillcombos]];


(*customization for mixed list of all 4* combined with my 5* *)

topIn45Skill[{skill1_String,skill2_String},howmany_Integer:4]:=Module[{all4skills,my5skills,thiscrewskills,crewwithskills,shuttleskilltotals},


all4skills=Block[{$UseCurrentSkills=False},AssociationMap[crewBonusSkills,stargroup[4]]];
my5skills=Block[{$UseCurrentSkills=False},AssociationMap[crewBonusSkills,mycrew[5]]];
thiscrewskills=Join[all4skills,my5skills];
crewwithskills=Select[thiscrewskills,And[#[skill1]["Base"]>0,#[skill2]["Base"]>0]&];shuttleskilltotals=skillShuttleTotal[#,{skill1,skill2}]&/@crewwithskills;

Take[ReverseSort[shuttleskilltotals],UpTo[howmany]]
];

topIn45Skill[{skill1_String},howmany_Integer:4]:=Module[{all4skills,my5skills,thiscrewskills,crewwithskills,shuttleskilltotals},

all4skills=Block[{$UseCurrentSkills=False},AssociationMap[crewBonusSkills,stargroup[4]]];
my5skills=Block[{$UseCurrentSkills=False},AssociationMap[crewBonusSkills,mycrew[5]]];
thiscrewskills=Join[all4skills,my5skills];
crewwithskills=Select[thiscrewskills,#[skill1]["Base"]>0&];shuttleskilltotals=skillShuttleTotal[#,{skill1}]&/@crewwithskills;


Take[ReverseSort[shuttleskilltotals],UpTo[howmany]]
];


topIn45ShuttleSkills[howmany_Integer:4]:=Union[Flatten[Keys[topIn45Skill[#,howmany]]&/@shuttleskillcombos]];


End[]

EndPackage[]



