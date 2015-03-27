(* ::Package:: *)

BeginPackage["NetForceGraphic3D`"];

NetForceGraphic3D::usage="NetForceGraphic3D[\!\(\*
StyleBox[\"n\", \"TI\"]\),{\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\)},{\!\(\*SubscriptBox[\(F\), 
StyleBox[\"2\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\)},\!\(\*
StyleBox[\"\[Ellipsis]\", \"TR\"]\){\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"n\", \"TR\"]]\)}] 
evaluates the magnitudes \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\) at azimuthal angles \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\), and polar angles \!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\) of \[ScriptN] number of three-dimentional forces, generates a graphic displaying \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\), \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\) and \!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\), and a three-dimensional plot of the resulting system."

Begin["Private`"]

NetForceGraphic3D[M_,Nforces__]:=
Module[
{F,\[Theta],\[Phi],\[ScriptM]=M,Fx,Fy,Fz,forces,components,sumx,sumy,sumz,tops,FN,\[Theta]N,\[Phi]N},
Column[{
Grid[
{
{"Force (N)","Azimuthal Angle \[Theta](\[Degree])","Polar Angle \[Phi](\[Degree])"},
{
forces={Nforces};
Fx[F_,\[Theta]_,\[Phi]_]:=N[F Cos[(\[Pi] \[Phi])/180]Cos[(\[Pi] \[Theta])/180]];
Fy[F_,\[Theta]_,\[Phi]_]:=N[F Cos[(\[Pi] \[Phi])/180]Sin[(\[Pi] \[Theta])/180]];
Fz[F_,\[Theta]_,\[Phi]_]:=N[F Sin[(\[Pi] \[Phi])/180]];
components=Table[
{
Fx[forces[[L,1]],forces[[L,2]],forces[[L,3]]],
Fy[forces[[L,1]],forces[[L,2]],forces[[L,3]]],
Fz[forces[[L,1]],forces[[L,2]],forces[[L,3]]]
},
{L,1,\[ScriptM],1}
];
sumx=Sum[components[[L,1]],{L,1,\[ScriptM]}];
sumy=Sum[components[[L,2]],{L,1,\[ScriptM]}];
sumz=Sum[components[[L,3]],{L,1,\[ScriptM]}];
FN=Sqrt[sumx^2+sumy^2+sumz^2],
tops=Max[Append[Table[forces[[n,1]],{n,1,\[ScriptM],1}],{FN,0}]];
\[Theta]N=Which[sumx>0&&sumy>=0,180/\[Pi]*ArcTan[sumy/sumx],
sumx==0&&sumy>0,90,
sumx<0&&sumy>=0,180/\[Pi]*ArcTan[sumy/sumx]+180,
sumx<0&&sumy<0,180/\[Pi]*ArcTan[sumy/sumx]+180,
sumx==0&&sumy<0,270,
sumx>0&&sumy<0,180/\[Pi]*ArcTan[sumy/sumx]+360],
\[Phi]N=If[sumz==0,0,180/\[Pi] ArcSin[sumz/FN]]
}
},
Frame->All,ItemStyle->{Bold,Large}
],
Show[
{
ParametricPlot3D[tops/2 {{Cos[u],Sin[u],0},{Cos[u],0,Sin[u]},{0,Cos[u],Sin[u]}},{u,0,2\[Pi]},
PlotStyle->{Black,Dashed,Thickness[.001]},Boxed->False,Axes->False],
Graphics3D[
{{Opacity[.2],Mesh->True,Sphere[{0,0,0},tops/2]},
{Dashed,Table[Line[{{0,0,0},{tops/2 Cos[L],tops/2 Sin[L],0}}],{L,0,(3\[Pi])/2,\[Pi]/2}]},
{Dashed,Table[Line[{{0,0,0},{0,0,tops/2 Cos[L]}}],{L,0,\[Pi],\[Pi]}]},
Table[Arrow[{{0,0,0},{components[[L,1]],components[[L,2]],components[[L,3]]}}],{L,1,\[ScriptM]}],
{Thickness[.01],Red,Arrow[{{0,0,0},{FN Cos[(\[Pi] \[Phi]N)/180]Cos[(\[Pi] \[Theta]N)/180],FN Cos[(\[Pi] \[Phi]N)/180]Sin[(\[Pi] \[Theta]N)/180], FN Sin[(\[Pi] \[Phi]N)/180]}}]},
{Text["\!\(\*OverscriptBox[\(0  \[Degree]\), \(+x\)]\)",{(2tops)/3,0,0}],Text["\!\(\*OverscriptBox[\(90  \[Degree]\), \(+y\)]\)",{0,(2tops)/3,0}],Text["\!\(\*OverscriptBox[\(180  \[Degree]\), \(-x\)]\)",{-((2tops)/3),0,0}],Text["\!\(\*OverscriptBox[\(270  \[Degree]\), \(-y\)]\)",{0,-((2tops)/3),0}],Text["\!\(\*OverscriptBox[\(\(+90\) \[Degree]\), \(+z\)]\)",{0,0,(2tops)/3}],Text["\!\(\*OverscriptBox[\(\(-90\) \[Degree]\), \(-z\)]\)",{0,0,-((2tops)/3)}]}
}
]
},ImageSize->{800,800},BoxRatios->{1,1,1},AxesOrigin->{0,0,0},SphericalRegion->True,PlotRange->1.01{{-tops,tops},{-tops,tops},{-tops,tops}}
]
},
Center,Frame->All
]
]

End[]
EndPackage[];



