(* ::Package:: *)

BeginPackage["ListNetForce`"];

ListNetForce::usage="ListNetForce[{{\!\(\*SubscriptBox[
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
StyleBox[\"n\", \"TI\"]]\)}}] 
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
StyleBox[\"n\", \"TI\"]]\), and generates a list {\!\(\*SubscriptBox[
StyleBox[\"F\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\),\!\(\*SubscriptBox[
StyleBox[\"\[Theta]\", \"TI\"], 
StyleBox[\"net\", \"TR\"]]\)} for the resulting system."

Begin["Private`"]

ListNetForce[Nforces__]:=
Module[{F,\[Theta],\[ScriptM],Fx,Fy,forces=Nforces,components,sumx,sumy,FN,\[Theta]N},
{
\[ScriptM]=Length[forces];
Fx[F_,\[Theta]_]:=N[F Cos[(\[Pi] \[Theta])/180]];Fy[F_,\[Theta]_]:=N[F Sin[(\[Pi] \[Theta])/180]];components=Table[{Fx[forces[[L,1]],forces[[L,2]]],Fy[forces[[L,1]],forces[[L,2]]]},{L,1,\[ScriptM],1}];sumx=Sum[components[[L,1]],{L,1,\[ScriptM]}];
sumy=Sum[components[[L,2]],{L,1,\[ScriptM]}];
FN=Sqrt[sumx^2+sumy^2],
\[Theta]N=Which[
sumx>0&&sumy>=0,180 /\[Pi] ArcTan[sumy/sumx],
sumx==0&&sumy>0,90,
sumx<0&&sumy>=0,180 /\[Pi] ArcTan[sumy/sumx]+180,
sumx<0&&sumy<0,180 /\[Pi] ArcTan[sumy/sumx]+180,
sumx==0&&sumy<0,270,
sumx>0&&sumy<0,180 /\[Pi] ArcTan[sumy/sumx]+360
]
}
]

End[]
EndPackage[];



