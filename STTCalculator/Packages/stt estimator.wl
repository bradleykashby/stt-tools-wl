(* ::Package:: *)

BeginPackage["STTCalculator`"]



Begin["`Estimator`"]


hazSkillVariance=0.2;
hazSkillPerHour=1250;
hazAmPass=5;
hazAmFail=30;


estimateVoyage[{startAm_Integer,ps_Integer,ss_Integer,o1_Integer,o2_Integer,o3_Integer,o4_Integer}]:=Module[
{ticksPerCycle=28,
secondsPerTick=20,
cycleSeconds,
cyclesPerHour,
hazPerCycle=6,
activityPerCycle=18,
dilemmasPerHour=0.5,
activityAmPerHour,
minPerHour=60,
psChance=0.35,
ssChance=0.25,
osChance=0.1,
dilPerMin=5,
skillChances,
skills,
maxSkill,
tries,
skill,
chance,
passSkill,
skillRngRange,
lostRngProportion,
skillPassRngProportion,
failSkill,
amLeft,
timeLeft,
voyTime,
skillFailRngProportion,
deductinput
},

cycleSeconds=ticksPerCycle*secondsPerTick;cyclesPerHour=60*60/cycleSeconds;hazPerHour=hazPerCycle*cyclesPerHour-dilemmasPerHour;activityAmPerHour=activityPerCycle*cyclesPerHour;skillChances={psChance,ssChance,osChance,osChance,osChance,osChance};
skills={ps,ss,o1,o2,o3,o4};

maxSkill=Max[skills];

endVoySkill=maxSkill*(1+hazSkillVariance);(*max of highest skill*)

tries=0;

While[True,
tries++;
If[tries==100,
Print["Something went wrong! Check your inputs."];
Break[]
];

am=startAm;

deductinput=Table[{skills[[i]],skillChances[[i]]},{i,6}];
deductAM/@deductinput;

amLeft=am-endVoySkill/hazSkillPerHour*activityAmPerHour;timeLeft=amLeft/(hazPerHour*hazAmFail+activityAmPerHour);

voyTime=endVoySkill/hazSkillPerHour+timeLeft;

If[Abs[timeLeft]>.0001,
endVoySkill=(voyTime)*hazSkillPerHour;,
Break[]
]
];

(*display results*)

Return[UnitConvert[Quantity[voyTime,"Hours"],MixedUnit[{"Hours","Minutes","Seconds"}]]]
;
];


estimateVoyage[{startAm_Integer,ps_List,ss_List,o1_List,o2_List,o3_List,o4_List}]:=Module[
{ticksPerCycle=28,
secondsPerTick=20,
cycleSeconds,
cyclesPerHour,
hazPerCycle=6,
activityPerCycle=18,
dilemmasPerHour=0.5,
activityAmPerHour,
minPerHour=60,
psChance=0.35,
ssChance=0.25,
osChance=0.1,
dilPerMin=5,
skillChances,
skills,
maxSkill,
tries,
skill,
chance,
passSkill,
skillRngRange,
lostRngProportion,
skillPassRngProportion,
failSkill,
amLeft,
timeLeft,
voyTime,
skillFailRngProportion,
deductinput
},

cycleSeconds=ticksPerCycle*secondsPerTick;cyclesPerHour=60*60/cycleSeconds;hazPerHour=hazPerCycle*cyclesPerHour-dilemmasPerHour;activityAmPerHour=activityPerCycle*cyclesPerHour;skillChances={psChance,ssChance,osChance,osChance,osChance,osChance};
skills={ps,ss,o1,o2,o3,o4};

maxSkill=Max[skills];

endVoySkill=maxSkill;(*max of highest skill*)

tries=0;

While[True,
tries++;
If[tries==100,
Print["Something went wrong! Check your inputs."];
Break[]
];

am=startAm;

deductinput=Table[{skills[[i]],skillChances[[i]]},{i,6}];
deductAM/@deductinput;

amLeft=am-endVoySkill/hazSkillPerHour*activityAmPerHour;timeLeft=amLeft/(hazPerHour*hazAmFail+activityAmPerHour);

voyTime=endVoySkill/hazSkillPerHour+timeLeft;

If[Abs[timeLeft]>.0001,
endVoySkill=(voyTime)*hazSkillPerHour;,
Break[]
]
];

(*display results*)

Return[UnitConvert[Quantity[voyTime,"Hours"],MixedUnit[{"Hours","Minutes","Seconds"}]]]
;
];


deductAM[{sk_Integer,sc_Real}]:=Module[{skill=sk,chance=sc,passSkill,skillRngRange,lostRngProportion,skillPassRngProportion,failSkill,skillFailRngProportion},

passSkill=Min[endVoySkill,skill*(1-hazSkillVariance)];

skillRngRange=skill*hazSkillVariance*2;
lostRngProportion=0;
If[skillRngRange>0,
lostRngProportion=Max[0,Min[1,(skill*(1+hazSkillVariance)-endVoySkill)/skillRngRange]];
];

skillPassRngProportion=1-lostRngProportion*lostRngProportion;
passSkill+=skillRngRange*skillPassRngProportion/2;

am+=passSkill*chance/hazSkillPerHour*hazPerHour*hazAmPass;

(*skill amount for 100% hazard fail*) failSkill=Max[0,endVoySkill-skill*(1+hazSkillVariance)];

(*skill amount for RNG fail*)
 skillFailRngProportion=(1-lostRngProportion)^2;failSkill+=skillRngRange*skillFailRngProportion/2;

(*am lost for failing hazards*)am-=failSkill*chance/hazSkillPerHour*hazPerHour*hazAmFail;


];



deductAM[{sk_List,sc_Real}]:=Module[{skill=sk,chance=sc,passSkill,skillRngRange,lostRngProportion,skillPassRngProportion,failSkill,skillFailRngProportion},

passSkill=Min[endVoySkill,Min[skill]];

skillRngRange=Max[skill]-Min[skill];
lostRngProportion=0;
If[skillRngRange>0,
lostRngProportion=Max[0,Min[1,(Max[skill]-endVoySkill)/skillRngRange]];
];

skillPassRngProportion=1-lostRngProportion*lostRngProportion;
passSkill+=skillRngRange*skillPassRngProportion/2;

am+=passSkill*chance/hazSkillPerHour*hazPerHour*hazAmPass;

(*skill amount for 100% hazard fail*)
 failSkill=Max[0,endVoySkill-Max[skill]];

(*skill amount for RNG fail*)
 skillFailRngProportion=(1-lostRngProportion)^2;failSkill+=skillRngRange*skillFailRngProportion/2;

(*am lost for failing hazards*)am-=failSkill*chance/hazSkillPerHour*hazPerHour*hazAmFail;


];


End[];

EndPackage[]
