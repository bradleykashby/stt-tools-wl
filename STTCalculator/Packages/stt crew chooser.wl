(* ::Package:: *)

BeginPackage["STTCalculator`"]



Begin["`CrewChooser`"]


selectBestCrew[crewlist_List,howmany_Integer:4]:=Module[{voyagecalctime,voyagecrew,shuttlecalctime,shuttlecrew,mybest},
	voyagecalctime=AbsoluteTiming[
		voyagecrew=AssociationMap[
			buildVoyageCrew[#,crewlist]&
			,skillpairs]
			//Values//Keys//Values//Keys//Flatten//Union;]//First;
	shuttlecalctime=AbsoluteTiming[
		shuttlecrew=topInShuttleSkills[crewlist,howmany];]//First;
	mybest=Union[voyagecrew,shuttlecrew];

<|"SelectedCrew"->mybest,
"VoyageCalculationTime"->Round@UnitConvert[Quantity[voyagecalctime,"Seconds"],MixedUnit[{"Minutes","Seconds"}]],
"ShuttleCalculationTime"->Round@UnitConvert[Quantity[shuttlecalctime,"Seconds"],MixedUnit[{"Minutes","Seconds"}]]|>

]

reCalculate[list_List]:=Module[{selectedbestcrew},
	selectedbestcrew=selectBestCrew[list];
	{mybest,voyagecalctime,shuttlecalctime}=
		{selectedbestcrew["SelectedCrew"],ToString[selectedbestcrew["VoyageCalculationTime"]],ToString[selectedbestcrew["ShuttleCalculationTime"]]}
		];

recalculateButton[]:=Module[{},
		freezinggrid=activatinggrid=Spacer[15];
		mybest={};
		SessionSubmit[Module[{},
			reCalculate[thelist];
			statusmessage="Calculation complete.";
			freezinggrid:=freezingdataset;
			activatinggrid:=activatingdataset;]];
		statusmessage="Recalculating. . .";]

crewKeeperPanel:=DynamicModule[{
(*define initial statics*)
	voyagecalctime="0",
	shuttlecalctime="0",
	whichlist={},
	freezinggrid=Spacer[15],
	activatinggrid=Spacer[15]
	},
	
	statusmessage="Status goes here.";
	

(*output panel*)

Panel[Grid[{
	{
		InputField[Dynamic[whichlist],Hold[Expression]],
		Spacer[5],
		Item[Button["Recalculate",
			recalculateButton[];,ImageSize->Automatic],Alignment->Right]
	},{
		Row[{Dynamic["VCT: "<>voyagecalctime],Spacer[5],Dynamic["SCT: "<>shuttlecalctime]}],
		SpanFromLeft,
		SpanFromLeft
	},{
		Item[Style["Crew to Freeze:",Bold],Alignment->Center],
		Spacer[5],
		Item[Style["Crew to Activate:",Bold],Alignment->Center]
	},{
		Dynamic[freezinggrid,TrackedSymbols:>Full],
		SpanFromAbove,
		Dynamic[activatinggrid,TrackedSymbols:>Full]
	},{
		Dynamic[statusmessage],
		SpanFromLeft
	}},
	Alignment->{Center,Top}]]

,
	Initialization:>(
	thelist:=ReleaseHold[whichlist];
	activelist:=Select[thelist,ContainsAny[{crewstatus[#]},{"Active","Leveling"}]&];

	
	crewtofreeze:=Select[Complement[activelist,mybest],crewstatus[#]=="Active"&];
	crewtoactivate:=Select[mybest,crewstatus[#]=="Frozen"&];
	freezingdataset:=Dataset[AssociationMap[Button["Freeze",(setNewStatus[#,"Frozen"];statusmessage=#<>" has been frozen";)]&,crewtofreeze]];
	activatingdataset:=Dataset[AssociationMap[Button["Activate",(setNewStatus[#,"Active"];statusmessage=#<>" has been activated";)]&,crewtoactivate]];
	
)

]


End[]
EndPackage[]
