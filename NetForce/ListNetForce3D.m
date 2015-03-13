(* ::Package:: *)

BeginPackage["ListNetForce3D`"];

ListNetForce3D::usage="ListNetForce3D[{{\!\(\*SubscriptBox[
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
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"2\", \"TR\"]]\)},\!\(\*
StyleBox[\"\[Ellipsis]\", \"TR\"]\){\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Phi]\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\)}}] 
evaluates an \!\(\*
StyleBox[\"n\", \"TI\"]\)\!\(\*
StyleBox[\"-\", \"TI\"]\)length list of three-dimentional forces with elements of the form {\!\(\*
StyleBox[\"F\", \"TI\"]\),\!\(\*
StyleBox[\"\[Theta]\", \"TI\"]\),\!\(\*
StyleBox[\"\[Phi]\", \"TI\"]\)}, with magnitudes \!\(\*SubscriptBox[
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
StyleBox[\"n\", \"TI\"]]\), and generates a list {\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\),\!\(\*SubscriptBox[\(\[Phi]\), 
StyleBox[\"net\", \"TR\"]]\)} for the resulting system."

Begin["Private`"]

ListNetForce3D[Nforces__]:=
Module[
{F,\[Theta],\[Phi],\[ScriptM],Fx,Fy,Fz,forces=Nforces,components,sumx,sumy,sumz,FN,\[Theta]N,\[Phi]N},
{
\[ScriptM]=Length[forces];
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



