#!/usr/bin/env wolframscript

<< KnotTheory`

intList[x_] := Module[{n, order, newList},
   n = Length[x];
   order = Ordering[x];
   newList = Table[0, {jjj, 1, n}];
   Table[newList[[order[[jjj]]]] = jjj, {jjj, 1, n}];
   newList];

preCrossingSign[preCrossing_] := 
  Sign[preCrossing[[1]] + 
    preCrossing[[2]] - (preCrossing[[3]] + preCrossing[[4]])];

pairSign[pair_] := Sign[pair[[2]] - pair[[1]]];

midPoint[preCrossing_] := Module[{mainPair, secondaryPair, sign},
   mainPair = preCrossing[[;; 2]];
   secondaryPair = preCrossing[[3 ;;]];
   sign = preCrossingSign[Join[mainPair, secondaryPair]];
   sign*Max[sign*secondaryPair]];

preCrossingOrderValue[preCrossing_] := 
  Module[{mainPair, secondaryPair, mainPairDistance},
   mainPair = preCrossing[[;; 2]];
   secondaryPair = preCrossing[[3 ;;]];
   mainPairDistance = 
    pairSign[mainPair]*(mainPair[[2]] - mainPair[[1]]);
   pairSign[
     mainPair]*(midPoint[preCrossing] + 
      Max[0, -mainPairDistance*
        Sign[
         mainPair[[1]] + mainPair[[2]] - secondaryPair[[1]] - 
          secondaryPair[[2]]]])];

petalToStem[petalPermutation_] := Module[{oneIndex, stemPermutation},
   oneIndex = Position[petalPermutation, 1][[1, 1]];
   stemPermutation = 
    Join[{0}, petalPermutation[[oneIndex ;;]], 
     petalPermutation[[;; oneIndex - 1]]];
   stemPermutation];

biggerThan[value_, comparisonList_] := Module[{totalList},
   totalList = Join[{value}, comparisonList];
   totalList = Sort[totalList];
   Position[totalList, value][[1, 1]] - 1];

removePDKinks[codePD_] := 
  Module[{PD1, finalPD, elements, a, b, c, n, jjj},
   If[Length[codePD] <= 2, Return[{}]];
   finalPD = codePD;
   n = Length[finalPD];
   jjj = 1;
   While[jjj <= n && Length[finalPD] >= 3, 
    If[Length[DeleteDuplicates[finalPD[[jjj]]]] < 4,
     elements = DeleteDuplicates[finalPD[[jjj]]];
     a = DeleteDuplicates[finalPD[[jjj]]][[1]];
     b = DeleteDuplicates[finalPD[[jjj]]][[2]];
     c = DeleteDuplicates[finalPD[[jjj]]][[3]];
     finalPD = DeleteCases[finalPD, finalPD[[jjj]]];
     Table[
      Table[
       If[MemberQ[elements, finalPD[[iii, mmm]]],
        finalPD[[iii, mmm]] = Min[elements],
        finalPD[[iii, mmm]] = 
         finalPD[[iii, mmm]] + 
          Min[0, -biggerThan[finalPD[[iii, mmm]], elements] + 1]]
       , {mmm, 1, Length[finalPD[[iii]]]}]
      , {iii, 1, Length[finalPD]}];
     n--,
     jjj++
     ]];
     If[Length[finalPD] <= 2, Return[{}]];
     finalPD];
   
removeAllR1[pdCode_] := Module[{finalPD, n},
   finalPD = pdCode;
   n = Length[finalPD] + 1;
   While[Length[finalPD] < n && Length[finalPD] >= 3, 
    n = Length[finalPD];
    finalPD = removePDKinks[finalPD]];
   If[Length[finalPD] <= 2, Return[{}]];
   finalPD];

removeR2[codePD_] := 
  Module[{PD1, finalPD, jjj, iii, n, elements, underStrand, 
    overStrand, r2Removed},
   If[Length[codePD] <= 2, Return[{}]];
   finalPD = removeAllR1[codePD];
   If[Length[finalPD] <= 2, Return[{}]];
   n = Length[finalPD];
   jjj = 1;
   While[jjj <= n,
    iii = jjj + 1;
    While[iii <= n,
     If[Length[Intersection[finalPD[[iii]], finalPD[[jjj]]]] >= 
        2 && (finalPD[[iii, 1]] == finalPD[[jjj, 3]] || 
         finalPD[[iii, 3]] == finalPD[[jjj, 1]]),
      underStrand = 
       DeleteDuplicates[{finalPD[[jjj, 1]], finalPD[[jjj, 3]], 
         finalPD[[iii, 1]], finalPD[[iii, 3]]}];
      overStrand = 
       DeleteDuplicates[{finalPD[[jjj, 2]], finalPD[[jjj, 4]], 
         finalPD[[iii, 2]], finalPD[[iii, 4]]}];
      elements = 
       DeleteDuplicates[
        Join[DeleteCases[underStrand, Min[underStrand]], 
         DeleteCases[overStrand, Min[overStrand]]]];
      finalPD = DeleteCases[finalPD, finalPD[[iii]]];
      finalPD = DeleteCases[finalPD, finalPD[[jjj]]];
      Table[
       Table[
        If[MemberQ[overStrand, finalPD[[kkk, mmm]]],
         finalPD[[kkk, mmm]] = 
          Min[overStrand] + 
           Min[0, -biggerThan[Min[overStrand], underStrand] + 1],
         If[MemberQ[underStrand, finalPD[[kkk, mmm]]],
          finalPD[[kkk, mmm]] = 
           Min[underStrand] + 
            Min[0, -biggerThan[Min[underStrand], overStrand] + 1],
          finalPD[[kkk, mmm]] = 
           finalPD[[kkk, mmm]] - 
            biggerThan[finalPD[[kkk, mmm]], elements]]]
        , {mmm, 1, Length[finalPD[[kkk]]]}]
       , {kkk, 1, Length[finalPD]}];
      If[Length[finalPD] <= 2, Return[{}]];
      finalPD = removeAllR1[finalPD];
      If[Length[finalPD] <= 2, Return[{}]];
      n = Length[finalPD];
      jjj = 1;
      iii = jjj + 1,
      iii++
      ]
     ];
    jjj++
    ];
   finalPD];

simplifyPDCode[pdCode_] := Module[{finalPD, n},
   finalPD = pdCode;
   n = Length[finalPD] + 1;
   While[Length[finalPD] < n,
    n = Length[finalPD];
    (*finalPD=removeAllR1[finalPD];*)
    finalPD = removeR2[finalPD]];
   finalPD];

permutationToPD[floatPermutation_] := 
  Module[{permutation, crossingCounter, sideCounter, 
    preCrossingSubList, mainPair, secondaryPair, key, reverseKey, 
    crossingList, preCrossingList, preCrossInfo, w, preCrossCopy, n},
   If[Mod[Length[floatPermutation], 2] != 1, 
    Print["The length of the permutation should be odd."];
    Return[4]];
   permutation = intList[floatPermutation];
   permutation = petalToStem[permutation];
   n = Length[permutation];
   preCrossingList = {};
   crossingList = <||>;
   crossingCounter = 1;
   sideCounter = 0;
   Table[
    preCrossingSubList = {};
    mainPair = {permutation[[jjj + 1]], 
      permutation[[Mod[jjj + 1, n] + 1]]};
    Table[
     secondaryPair = {permutation[[Mod[jjj + 2 + 2*iii, n] + 1]], 
       permutation[[Mod[jjj + 3 + 2*iii, n] + 1]]};
     If[(mainPair[[1]] - secondaryPair[[1]])*(mainPair[[2]] - 
          secondaryPair[[1]])*(mainPair[[1]] - 
          secondaryPair[[2]])*(mainPair[[2]] - secondaryPair[[2]]) <= 
       0,
      key = Join[mainPair, secondaryPair];
      reverseKey = Join[secondaryPair, mainPair];
      If[Not[KeyExistsQ[crossingList, reverseKey]],
       crossingList[key] = {sideCounter}];
      preCrossingSubList = 
       Join[preCrossingSubList, {Join[mainPair, secondaryPair]}]
      ],
     {iii, 0, n/2 - 2}];
    preCrossingSubList = 
     Sort[preCrossingSubList, 
      preCrossingOrderValue[#1] < preCrossingOrderValue[#2] &];
    preCrossingList = Join[preCrossingList, preCrossingSubList];
    sideCounter = Mod[sideCounter + 1, 2],
    {jjj, 0, n - 1}];
   Table[
    key = preCross;
    reverseKey = Join[preCross[[{3, 4}]], preCross[[{1, 2}]]];
    If[KeyExistsQ[crossingList, key],
     crossingList[key] = Append[crossingList[key], crossingCounter],
     crossingList[reverseKey] = 
      Append[crossingList[reverseKey], crossingCounter]];
    crossingCounter++,
    {preCross, preCrossingList}];
   preCrossingList = Keys[crossingList];
   Table[
    key = preCrossingList[[jjj]];
    preCrossInfo = crossingList[key];
    w = Sign[(preCrossingList[[jjj, 2]] - 
          preCrossingList[[jjj, 1]])*(preCrossingList[[jjj, 4]] - 
          preCrossingList[[jjj, 3]])] + 
      Sign[preCrossingList[[jjj, 3]] + preCrossingList[[jjj, 4]] - 
        preCrossingList[[jjj, 1]] - preCrossingList[[jjj, 2]]];
    preCrossingList[[jjj, 1]] = 
     Max[Mod[preCrossInfo[[2]], crossingCounter], 1];
    preCrossingList[[jjj, 2]] = 
     Max[Mod[preCrossInfo[[2]] + 1, crossingCounter], 1];
    preCrossingList[[jjj, 3]] = 
     Max[Mod[preCrossInfo[[3]], crossingCounter], 1];
    preCrossingList[[jjj, 4]] = 
     Max[Mod[preCrossInfo[[3]] + 1, crossingCounter], 1];
    If[preCrossInfo[[1]] == 0,
     preCrossCopy = preCrossingList[[jjj]];
     preCrossingList[[jjj, 3]] = preCrossCopy[[2]];
     preCrossingList[[jjj, Mod[1 + w, 4] + 1]] = preCrossCopy[[3]];
     preCrossingList[[jjj, Mod[3 - w, 4] + 1]] = preCrossCopy[[4]],
     preCrossCopy = preCrossingList[[jjj]];
     preCrossingList[[jjj, 1]] = preCrossCopy[[3]];
     preCrossingList[[jjj, 3]] = preCrossCopy[[4]];
     preCrossingList[[jjj, Mod[1 + w, 4] + 1]] = preCrossCopy[[1]];
     preCrossingList[[jjj, Mod[3 - w, 4] + 1]] = preCrossCopy[[2]]
     ],
    {jjj, 1, Length[preCrossingList]}];
   simplifyPDCode[preCrossingList]];
   

If[Length[$ScriptCommandLine]>=2,
inputName=ToString[$ScriptCommandLine[[2]]];
fileName = StringJoin["petalKnotTable_",inputName, ".csv"],
fileName = 
  StringJoin["petalKnotTable_", 
   StringReplace[DateString["ISODateTime"], ":"->"-"], ".csv"]];


coeffPairs[pol_, x_Symbol] := {#, Coefficient[pol, x, #]} & /@ 
   Exponent[pol, x, List];
   
   
coeffTriples[pol_, x_Symbol, y_Symbol] := 
 Module[{exponents1, exponents2, exponentPairs, coefficientTriples},
  exponents1 = Exponent[pol, x, List];
  exponents2 = Exponent[pol, y, List];
  exponentPairs = Tuples[{exponents1, exponents2}];
  coefficientTriples = 
   Table[{exponentPairs[[jjj, 1]], exponentPairs[[jjj, 2]], 
     Coefficient[Coefficient[pol, y, exponentPairs[[jjj, 2]]], x, 
      exponentPairs[[jjj, 1]]]},
    {jjj, 1, Length[exponentPairs]}];
  coefficientTriples = 
   Sort[DeleteCases[coefficientTriples, {_, _, 0}]];
  coefficientTriples];   
   

newHeader = 
  StringJoin["permutation", ",", "PD_code", ",", "Khovanov_polynomial", 
   ",","Jones_polynomial", 
   ",", "Jones_polynomial_span", ",", "Alexander_polynomial", ",", 
   "Alexander_polynomial_span", ",", "Signature", ",", 
   "Determinant"];
outputFileName = fileName;

WriteLine[outputFileName, newHeader];

n = 50000;
Table[PDCode = Table[-1, {jjj, 1, 61}];
 While[Length[PDCode] > 40,
  Check[
   halfPetalNumber = RandomInteger[{10, 20}];
   petalPermutation = 
    RandomVariate[NormalDistribution[0, 1], 2*halfPetalNumber + 1];
   PDCode = permutationToPD[petalPermutation];
   If[Length[PDCode]<=2, 
     PDCode = {{8, 3, 1, 4}, {7, 5, 8, 4}, {5, 2, 6, 3}, {6, 2, 7, 1}}],
   Print["Failed at step 1 on petal permutation ", petalPermutation]]];
 Check[
  petalString = 
    StringReplace[ToString[petalPermutation], {"{" -> "[", "}" -> "]"}];
  PDString = StringReplace[ToString[PDCode], {"{" -> "[", "}" -> "]"}];
   khovanovHomology = Kh[PD @@ (X @@@ PDCode)][q, t]; 
   khovanovPolynomial = Sort[coeffTriples[khovanovHomology, q, t]]; 
   jonesPolynomial = 
    Sort[coeffPairs[
      Simplify[Factor[khovanovHomology /. t -> -1]/(q + q^(-1))] /.q -> q^(1/2), q], #1[[1]] < #2[[1]] &];
   khovanovString = 
    StringReplace[
     ToString[khovanovPolynomial], {"{" -> "[", "}" -> "]"}];
   jonesSpan = 
    ToString[jonesPolynomial[[-1, 1]] - jonesPolynomial[[1, 1]]];
   jonesString = 
    StringReplace[ToString[jonesPolynomial], {"{" -> "[", "}" -> "]"}];
   alexanderPolynomial = 
    Sort[coeffPairs[Alexander[PD @@ (X @@@ PDCode)][q], 
      q], #1[[1]] < #2[[1]] &];
   alexanderSpan = 
    ToString[
     alexanderPolynomial[[-1, 1]] - alexanderPolynomial[[1, 1]]];
   alexanderString = 
    StringReplace[
     ToString[alexanderPolynomial], {"{" -> "[", "}" -> "]"}];
   signature = ToString[KnotSignature[PD @@ (X @@@ PDCode)]];
   determinant = ToString[KnotDet[PD @@ (X @@@ PDCode)]];
   newLineTemp = 
    StringJoin["\"", petalString, "\"", ",", "\"", PDString, "\"", 
     ",", "\"", khovanovString, "\"", ",", "\"", jonesString, "\"", 
     ",", "\"", jonesSpan, "\"", ",", "\"", alexanderString, "\"", 
     ",", "\"", alexanderSpan, "\"", ",", "\"", signature, "\"", ",", 
     "\"", determinant, "\""];
   WriteLine[outputFileName, newLineTemp],
   Print["Failed at step 2 on petal permutation ", 
    petalPermutation]];
  If[Mod[iii, 100] == 0, Print["Finished ", iii, " knots."]], 
{iii, 1, n}];

Print["Finished creating ", fileName, "!"];
RenameFile[fileName, StringJoin["finished_", fileName]];