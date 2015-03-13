(* ::Package:: *)

BeginPackage["NetForce3D`"];

NetForce3D::usage="NetForce3D[\!\(\*
StyleBox[\"n\", \"TI\"]\),{\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"1\", \"TR\"]]\)},{\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], \(2\)]\)},\!\(\*
StyleBox[\"\[Ellipsis]\", \"TR\"]\){\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\)}] 
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
StyleBox[\"n\", \"TI\"]]\) of \[ScriptN] number of three-dimentional forces, and generates a list {\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\)} for the resulting system."

Begin["Private`"]

NetForce3D[M_,Nforces__]:=
Module[
{F,\[Theta],\[Phi],\[ScriptM]=M,Fx,Fy,Fz,forces,components,sumx,sumy,sumz,FN,\[Theta]N,\[Phi]N},
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
\[Theta]N=Which[sumx>0&&sumy>=0,180/\[Pi]*ArcTan[sumy/sumx],
sumx==0&&sumy>0,90,
sumx<0&&sumy>=0,180/\[Pi]*ArcTan[sumy/sumx]+180,
sumx<0&&sumy<0,180/\[Pi]*ArcTan[sumy/sumx]+180,
sumx==0&&sumy<0,270,
sumx>0&&sumy<0,180/\[Pi]*ArcTan[sumy/sumx]+360],
\[Phi]N=If[sumz==0,0,180/\[Pi] ArcSin[sumz/FN]]
}
]

End[]
EndPackage[];



