(* ::Package:: *)

BeginPackage["NetForce`"];

NetForce::usage="NetForce[\!\(\*
StyleBox[\"n\", \"TI\"]\),{\!\(\*SubscriptBox[
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
evaluates the magnitudes \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\) at angles \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\) to \!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\) of \[ScriptN] number of forces, and generates a list {\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\)} for the resulting system."

Begin["Private`"]

NetForce[M_,Nforces__]:=
Module[
{F,\[Theta],\[ScriptM]=M,Fx,Fy,forces,components,sumx,sumy,FN,\[Theta]N},
{
forces={Nforces};
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
\[Theta]N=Which[sumx>0&&sumy>=0,180/\[Pi]*ArcTan[sumy/sumx],
sumx==0&&sumy>0,90,
sumx<0&&sumy>=0,180/\[Pi]*ArcTan[sumy/sumx]+180,
sumx<0&&sumy<0,180/\[Pi]*ArcTan[sumy/sumx]+180,
sumx==0&&sumy<0,270,
sumx>0&&sumy<0,180/\[Pi]*ArcTan[sumy/sumx]+360]
}
]

End[]
EndPackage[];



