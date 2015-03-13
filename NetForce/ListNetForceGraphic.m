(* ::Package:: *)

BeginPackage["ListNetForceGraphic`"];

ListNetForceGraphic::usage="ListNetForceGraphic[{\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\)},{\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\)},\!\(\*
StyleBox[\"\[Ellipsis]\", \"TR\"]\){\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\)}] 
evaluates an \!\(\*
StyleBox[\"n\", \"TI\"]\)\!\(\*
StyleBox[\"-\", \"TI\"]\)length list of forces with elements of the form {\!\(\*
StyleBox[\"F\", \"TI\"]\),\!\(\*
StyleBox[\"\[Theta]\", \"TI\"]\)}, with magnitudes \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\) at angles \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\), generates a graphic displaying \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\) and \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\), and a plot of the resulting system."

Begin["Private`"]

ListNetForceGraphic[Nforces__]:=
Module[{F,\[Theta],\[ScriptM],Fx,Fy,forces=Nforces,components,sumx,sumy,FN,\[Theta]N,tops},
Column[{
Grid[
{
{"Force (N)","Angle (\[Degree])"},
{
\[ScriptM]=Length[forces];
Fx[F_,\[Theta]_]:=N[F Cos[(\[Pi] \[Theta])/180]];
Fy[F_,\[Theta]_]:=N[F Sin[(\[Pi] \[Theta])/180]];
components=Table[
{
Fx[forces[[L,1]],forces[[L,2]]],
Fy[forces[[L,1]],forces[[L,2]]]
},
{L,1,\[ScriptM],1}
];
sumx=Sum[components[[L,1]],{L,1,\[ScriptM]}];
sumy=Sum[components[[L,2]],{L,1,\[ScriptM]}];
FN=Sqrt[sumx^2+sumy^2],
tops=Max[Append[Table[forces[[n,1]],{n,1,\[ScriptM],1}],{FN,0}]];
\[Theta]N=Which[sumx>0&&sumy>=0,180/\[Pi] ArcTan[sumy/sumx],
sumx==0&&sumy>0,90,
sumx<0&&sumy>=0,180/\[Pi] ArcTan[sumy/sumx]+180,
sumx<0&&sumy<0,180/\[Pi] ArcTan[sumy/sumx]+180,
sumx==0&&sumy<0,270,
sumx>0&&sumy<0,180/\[Pi] ArcTan[sumy/sumx]+360]
}
},
Frame->All,ItemStyle->{Bold,Large}
],
Show[
Graphics[
{Circle[{0,0},tops/2],
{Dashed,Table[Line[{{0,0},{tops/2 Cos[L],tops/2 Sin[L]}}],{L,0,(3\[Pi])/2,\[Pi]/2}]},
Table[Arrow[{{0,0},{Fx[forces[[L,1]],forces[[L,2]]],Fy[forces[[L,1]],forces[[L,2]]]}}],{L,1,\[ScriptM]}],
{Thickness[.01],Red,Arrow[{{0,0},{FN Cos[(\[Pi] \[Theta]N)/180],FN Sin[(\[Pi] \[Theta]N)/180]}}]},
{Text["\!\(\*OverscriptBox[\(0  \[Degree]\), \(+x\)]\)",{(2tops)/3,0}],Text["\!\(\*OverscriptBox[\(90  \[Degree]\), \(+y\)]\)",{0,(2tops)/3}],Text["\!\(\*OverscriptBox[\(180  \[Degree]\), \(-x\)]\)",{-((2tops)/3),0}],Text["\!\(\*OverscriptBox[\(270  \[Degree]\), \(-y\)]\)",{0,-((2tops)/3)}]}
}
],
ImageSize->400,AspectRatio->1,PlotRange->{{-((5tops)/4),(5tops)/4},{-((5tops)/4),(5tops)/4}}]
},
Center,Frame->All
]
]

End[]
EndPackage[];



