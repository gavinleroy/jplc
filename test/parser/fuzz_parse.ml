(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let%expect_test "fuzz-1" =
  Ppp.ppp_ast
    "
    // OK

    let x0 = !R4lwK5;
    let x1 = --xFLl2U>=--37824*F7ngeu+eio7K7/!19406||gV8NYQ<=Ww_fFC;
    let x2 = ((if (-41104) then 2500
      else
      (WznWXZ))>=(((6988)))>!28236)>(if
      vpU642-((MEVD_u)) then
      !Udneg8<=(!33397)
      else -25933>26956);
    let x3 = !(-10409||-27992)>(15342)>HR_60W<!-15433!=!(25256)<=CEZIYq;
    let x4 = e58OgM;
    let x5 = -26642*!CwCmnv-(if vZkfak then WPKTMm
      else b1cp0k)==(-11038>=(W90iHJ))<=(if IGlbzW then DQ_qtV
      else NOj800);
    let x6 = !(VpW3EG)>-32163==--15524==FnXS9U<=rtpOqy/(49796)/28973==Rksl99;
    let x7 = -35960==-30757<!(Rr_t5j)+KXT7Hh>vHipMF;
    let x8 = (-FfMo8T>=PGpuFd!=(if (--37672) then bEJPDe else sRr2uk))-QEQJSG*-6428<=(42934);
    let x9 = zhrdSU||(-39172)<eVlubh%(if 36368 then QepUKv
      else (if -44736 then 46564
      else dpNR2d));";
  [%expect
    {|
      (Prog
       ((StmtCmd (LetStmt (ArgLValue (VarArg x0)) (UnopExpr ! (VarExpr R4lwK5))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x1))
          (IteExpr
           (BinopExpr (UnopExpr - (UnopExpr - (VarExpr xFLl2U))) >=
            (BinopExpr (BinopExpr (UnopExpr - (IntExpr -37824)) * (VarExpr F7ngeu))
             + (BinopExpr (VarExpr eio7K7) / (UnopExpr ! (IntExpr 19406)))))
           (TrueExpr) (BinopExpr (VarExpr gV8NYQ) <= (VarExpr Ww_fFC)))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x2))
          (BinopExpr
           (BinopExpr
            (BinopExpr (IteExpr (IntExpr -41104) (IntExpr 2500) (VarExpr WznWXZ))
             >= (IntExpr 6988))
            > (UnopExpr ! (IntExpr 28236)))
           >
           (IteExpr (BinopExpr (VarExpr vpU642) - (VarExpr MEVD_u))
            (BinopExpr (UnopExpr ! (VarExpr Udneg8)) <=
             (UnopExpr ! (IntExpr 33397)))
            (BinopExpr (IntExpr -25933) > (IntExpr 26956))))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x3))
          (BinopExpr
           (BinopExpr
            (BinopExpr
             (BinopExpr
              (UnopExpr ! (IteExpr (IntExpr -10409) (TrueExpr) (IntExpr -27992))) >
              (IntExpr 15342))
             > (VarExpr HR_60W))
            < (UnopExpr ! (IntExpr -15433)))
           != (BinopExpr (UnopExpr ! (IntExpr 25256)) <= (VarExpr CEZIYq)))))
        (StmtCmd (LetStmt (ArgLValue (VarArg x4)) (VarExpr e58OgM)))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x5))
          (BinopExpr
           (BinopExpr (BinopExpr (IntExpr -26642) * (UnopExpr ! (VarExpr CwCmnv)))
            - (IteExpr (VarExpr vZkfak) (VarExpr WPKTMm) (VarExpr b1cp0k)))
           ==
           (BinopExpr (BinopExpr (IntExpr -11038) >= (VarExpr W90iHJ)) <=
            (IteExpr (VarExpr IGlbzW) (VarExpr DQ_qtV) (VarExpr NOj800))))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x6))
          (BinopExpr
           (BinopExpr
            (BinopExpr (BinopExpr (UnopExpr ! (VarExpr VpW3EG)) > (IntExpr -32163))
             == (UnopExpr - (IntExpr -15524)))
            ==
            (BinopExpr (VarExpr FnXS9U) <=
             (BinopExpr (BinopExpr (VarExpr rtpOqy) / (IntExpr 49796)) /
              (IntExpr 28973))))
           == (VarExpr Rksl99))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x7))
          (BinopExpr (IntExpr -35960) ==
           (BinopExpr
            (BinopExpr (IntExpr -30757) <
             (BinopExpr (UnopExpr ! (VarExpr Rr_t5j)) + (VarExpr KXT7Hh)))
            > (VarExpr vHipMF)))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x8))
          (BinopExpr
           (BinopExpr
            (BinopExpr
             (BinopExpr (UnopExpr - (VarExpr FfMo8T)) >= (VarExpr PGpuFd)) !=
             (IteExpr (UnopExpr - (IntExpr -37672)) (VarExpr bEJPDe)
              (VarExpr sRr2uk)))
            - (BinopExpr (VarExpr QEQJSG) * (IntExpr -6428)))
           <= (IntExpr 42934))))
        (StmtCmd
         (LetStmt (ArgLValue (VarArg x9))
          (IteExpr (VarExpr zhrdSU) (TrueExpr)
           (BinopExpr (IntExpr -39172) <
            (BinopExpr (VarExpr eVlubh) %
             (IteExpr (IntExpr 36368) (VarExpr QepUKv)
              (IteExpr (IntExpr -44736) (IntExpr 46564) (VarExpr dpNR2d)))))))))) |}]

(* let%expect_test "fuzz-2" =
 *   Ppp.ppp_ast
 *     "
 *     // OK
 *
 *     let x0 = ((!!-20499*-(if bIQ92g/UIdxdO then (44156)!=r7b9iJ else !35068<=!lmbdqa)/!(if 45002 then -33250 else -34165)+-42150/20808==(if XakKbD then (if -YfTHzG>(fBCldn) then -39305-3955 else (--29763)) else ((RjAI5H)))||(--8251)&&-2524%TQDRFD<=(SkRqtw)==zVwoIF<(!ku4ftk<=RicSNi-!!14897>nG497I)==(if eU8CYN<=((GOTbEd))*!-30355+TXleBU then (if -(-27936)!=(i2O1hT) then kvxiLm*KS_Ds7 else IDvUE1*7396) else (if iHpmEC/(42209) then V7n9Bh%!ZsKqnE else (if (bm4ufY) then -30825 else -OU6hno))))%-29169)+(if (((if 33071 then WogMes else s46tQt)<=((((42050)>=DjmCd_)))<=(if zb9mmv-Tt2GAS then --39919!=-10694 else ((-47961))!=(t63zZj))+-vGuMK_--4669||3376==(if !ROuaFG>(w2xy2d) then -s3MHuV/12766 else xxUgLs+(38261))<MYvqbn))&&(if !-29273 then 45571+KqH43R-(16169)>=(!-31835==(--15565)<=49806) else N1J4Wc) then !x0m3Qe>(if TN9_WT then (-!31632>=-22093+(if (-24516) then (-36187) else !TrEmoZ)) else (Cz0HX8!=((-15720)/-mTa0NG)))!=!rFY3DK>=-257>qLWudh<-19850<(xieWKX)<=-25491-((CSAQVk))&&kthsYh!=kTZB5s%-!-RGY5sE||AV42A1-vsCpKl>=-24493+U2KqpX!=-45335||45733 else HvRbcd!=(((-36784/18679+-26340||thUwtR))==-10838/(if lKodCk then B6Iyrk else ((!22789)))-31944)/-44219*(-(if aPRhla>=!12116 then !-42986<=(jON3DY) else Pv9T7q<=15982)/!13756==(-piDTZ3)));
 *     let x1 = -((I1gXyk))%(if -LwvZz5>=-!Qja7U7!=(19077)&&ztZdeW==xIKIyO+36118<=16688<-ck7RFm==16676!=(if -7898 then (-PL4wzV) else (--6490))<wjSv3i<(46984)/18718--11750 then (Iz1AzQ) else XplVG3%-12673>((426)||-2768)<((8399)<-44743)+EvCF2x%b8upKM&&-5680>=(7361!=--43472)>=(GQ7HwE))-W7aKjx+(((if (((!8969!=(if ((14807)>c5dnt8)<=QucVkR<pgfnsZ then ((if dNufqz then 12903 else (-VnJugB))<22063) else ULO9Gs*-27984==7210!=!PJ2Qlp)!=!((!--42632==(33925)+(if V4INFU then !cDxuBR else 23971)))+17211!=--24381!=(!26175)==(-MzDYuQ<=(G4IZuU)!=(if 35609 then ye2Uvx else -SmvGzH))==!36748))) then EqW8Z1 else 35029>!(--24642/voFGIn)&&(!-38948*!1264)&&-1464||(if !rZn5xT*(if 2027 then -45346 else -27707)%(if 39319 then -22326 else Kwc0fi) then (nCclBN-(rUC8jP)%(-47313)||10929>--13066) else 3571))>(if !(XNsklt&&!((-1986==17528))<=(((yDYgno)))/!XQGkDX)+(if !JRxMeR/-21368%-19757 then !ZBwNSp||-10084>6258 else jK1KOQ!=-37748>(GrVaaO!=(-31192))) then !37118||-17683 else -25270>=!!XON7uX/VT6L_d<JCvipT>=gFYGKL-(if -SJNhyk>=-19326 then -6214>=WfjsfB else -DYPMtG)*pboNIL<=(M_dbmU)<=ir5lMw*16322)<=(if -(if (if 19975 then !s6Y3YZ else gufuiw)-GVb8Pt then !-HvYDIG-KwrRnr>=(pNbk30)-275 else JA_fAF||-JG203l%22836||16182)+-40068 then -(if (PDucEM) then CwWLzv>=!BJYoQG else -sRtkA7<!19041)-5685-47204<=(tXtQfT<XsGrme)>=!!-3937>-23348<(if -23001 then s4N9dy else !-14442)&&-40394 else (-25353)<38463)));
 *     let x2 = (if -(if -42561 then w4CMeD else ftcpAE)>(if -30785 then (Xdx2sZ) else 15997)>-HrsYVU+-47821%-41684%-rMXHgw<(if -(if -20798 then (!TOViOV) else 2065)==-(oYMkus)!=bbmuda then (!!Y9xdJH==!12816!=PEwaTd) else -41980)*lTxcQs*((uTFS4c))+(-zAPfZN<(FiDVwi))-31000<=((n7lekz))||((-47609))<(!lBw0Z0)&&--2271<(!-43814)%!37587*(34481)%(-kI_2q5*U4GiLo>(if 36868 then -19414 else (30872)))>!-(lMx17j)*-3417||((-10256))%-5877>=--TEkeOh==-14493!=25198*(((if (29302) then (aUWfzU) else !UKxrd1)/GB9wRQ+-40957))>=(if (-44110)>!KYHl44 then -cnP9t1+45560 else OMIizV>=Wa3j46)>=--24914||33801+EsB63O then ((!(if -46422+!-43574&&(!CjOiNf>=RestYh) then (14432) else (if -3019 then (-bg2tbR) else -34047)%15288<=K9c7Wt)<=N_M1YX*!19264/k4bICv/GiRcrC<(if (-Y7n66c-32344||WbzCBH/45383)>=(39933<(if -34338 then cAJTDo else 1092)) then (if (if k2LVzc then !20233 else !42272) then o9XQn9 else (((LUTPyI))))<BYf7XS else !-30443>=(z9gDc8)*-24226<=!13748>=Nr6Vmo)))*(if (if (if (if o7jYkz then SbhxAQ else nfp5vA) then !19506+La0mWB else !!LpnBPA-(!KxyTKK)) then CoFEUv else (if -18389<=(15309) then (if -25880 then BCZ1Ax else 30027) else KXufhd))*RHHgvA+!19039==RvX_uv>=MqiHoQ==(if lEFQcT!=((AC4Y5I)) then -22173 else ((awrtim)==-27595)) then (-44886) else -3309!=AFMO1D-AiV9tN%11745!=45071<-13440>(if -46991 then tAjxXN else -uyOzF6)<=-32425&&-(--17396)/(36706)&&!-15130||(9227&&26659)) else (if (-23404)+Tl6dLC&&fHf0Cj>=-9187-!DcFn3v==pmESjp%-25226 then 35409<!(ACC25y)||(((36616))*MYxB7q/49022)!=(bIXYyS)==--25620%Pg50Rr else ((!iCvtd1>=-17427!=(if (if -1857 then 39621 else -1283)==!uxADgc>(-2780>40562>URl4sj) then (if (-39186/-45733+((-BOi8AT<=-30381))) then -G2iC9x else (3220)) else (if -44357*(!X5NH2_)||(6478)>=(-45516) then (OKSw3i==AY3n0G)<37856||-49836 else (if -33518 then -jsjMS8 else MopAGA)>=sr97an<=(21821)))))))>-(if (!38062)/(31926) then ((-30660)>=13641) else (17197!=gy_u1t))<=bHMu3m==((if ((34490)) then -10649 else -19091)<(((r1a4_G)<(T2RwkB))))>(!(9820)<-22236)/28158>(if ((!(if (18324)!=FE95JS then (if -41133 then !W8M3Gf else TCTpW2) else O3UrEb<-FPEmrf)>=!(((pAT1AX)>=20553))<=(if -29719 then -1915 else rAWuKI))) then (!SLbt8u)>=(if (if 7160 then -4901 else 42035) then nWhskH else !Q4jmhF) else !-THSuOl==-38614/(if -10550 then -49437 else -16527)<=-44992>-16643||(-36697))<((aGZrmC)>=(21197)-47143/(if -6108 then Q0p5yE else --36736)-!(-10522!=-32908)<=!Rvwkow&&!-9181)==!27021==-38315%WEiTBT>=Qs7qLE%-3311||(-44147)||fvPQnI<=(((if MkQX3M then kzE5vV else 8264)%((ss2T09))/(UxGBtS))&&(CJxOk_)+oEK04K)||(((((iLJMZA/(if !-23415 then 9197 else fFTGlF))<-16394*uvJ1Cd-ez6Ohr!=!17473)))*(if (-((!16662))<=-44232) then wxnXIg*-4534 else -17056%29849)||(if (!4710) then APyTQR/(-10849) else (if 25925 then 11446 else foaEea)));
 *     let x3 = (if !-(if mSE9xY then BrtMxW-!((C3_JBv!=(((sHDFY4)))))&&(3451>-2712) else (if ID0HcA<38431+HCaNS2 then XbskwB else -39954+cZ4B7G/((-32071))))-(if !35274 then (if ((mt5T8J!=47053==-6427<=LRi7IE)) then !RWW7u5||-28516==(if 25087 then 43843 else (tNrV2J)) else oSSxCj<=BZIQwZ%(-28750)) else (if (if !I5exYB then 15647 else 33879) then (-20423-32791) else Bl0Jvj)--38888==!G521Je>=Ef9r41%-MM6xgw)>=((if --7748 then (Eq2dQX||48944)<!!-44935<hWi7R7 else (2793-(!Dek5_C==PhQrMo)))/((vSPjHL-24029))%(20693)>43797/(10234)*-48353)!=!(udXXSd)!=(ck7O5U)>((-44666))||(-38171)*(if -5784 then (!34604<!4087) else (if KmTvr1 then 27754 else --33710))!=(!15362-14397%(1178)>=tzz5pN)+3913<=Fn2UMv then -(-3253)%-q5zI2P>=-12343||!-41435--13281%wAtU8f-(if (((!33522))<=-35397)==!(hlQuia)%(24981) then -(hCtI5u)&&32279&&(41213)-33801 else -20194)%onR1OS>(((-27542)))-(((if (if 15437 then K4WKmj else e9aLyw) then -22648%20735 else 38909<=-uZSViC)+Fg30cO==(if (if (ZpUcNj) then 29501 else glParx%-38662) then (if --1101>31633 then (if tG7qNk then 31886 else (((47774)))) else (WI2JiD)<(-16606)) else -DPT77H))<=-GPyQG6>!-39151-((((Np8SII))))||(DfYLhI!=tDmPlB)<=(((!(if 19386 then !-18414 else 20195)==(LAz5lE/vXN5mP))))) else ((if (if czgQwe==-SufmIS then 22607+zS4RVc else v6ThoH) then (((KtifxY)&&YTp0cU))*6060==-O9Y6ru else ((-Kbxe6s+Qelx4I))<6967)/(if !(-22526)*!vA3JWY then mFDhlC<=18087 else -797)<(if (IjZ41P) then Vhr9kr else !31479)<=(-15460<BjAlLz)<=-(if X9bJmt then (if !KrSweT then byqY7V else rD7Eax) else -F9JV2T>ux8tne)<=waIQ5m||(if (if (if LUdDq_ then VpL2zX else -39102) then (if (-18846) then lbHvad else RhzCnb) else (!39213)<--8722) then (if (if LB50hy then (npvydO) else (!30693)) then -((49052))*lS9vXD else -24281*MDBt1g) else -oZAGlZ))+(if ((49341)) then (-9401)+(if voD1HT<=(-17610) then XVeSd3/-47050 else 4938!=3906)%-nzVA88>=(!-679)/31316<u5zXP6 else (if (if !!usCYAf==(if (26563) then 18414 else dKKkUT) then (if ((48619)) then -2699 else EgDQY9)!=(if -27159 then -4976 else (vcMt3r)) else (-(yhI44d)&&(iXL3gG)/-28229)) then --31448+lCWhcg-NMnRDz/((-27369))%BITcJK/!IOZ3Vb<-33748!=-38869 else T8FfnA))||((10688)<=!(if (47027) then KfXHEb else H5wfGn)&&10360<(-VmCX5H<9954+!32138>=YUpCVB)&&(if ((2196--(17123)<=-43345)) then (PbBvcG)-eRobs5%--36383 else J4tnXB%!30811>=C2O0Ld))||LCDkah);
 *     let x4 = !oGjJXM%-10660--YXT1BM/pDbPVD!=!(if -21047<=ZAtNU2 then -eyAZsN<458==(IOXjqZ) else -44163)>=24344>(if (((!kWw6a6>=-15306<(Pk8WKF)))) then !M84piK!=-46319-(if ((33528)) then dXYFnU else -2619) else !-8939%((-20239))+((11471))==6741);
 *     let x5 = (if !((pUW0JR*((((((if -43967 then H6olxA else --7529)/(35133)||-23912)||(if 17201<=35563 then mjB2D3%(-8616) else (if !-27137 then ((24290)) else !34238))))))!=-HgzoIx>=(-16206)))-hWRaK_+--21972*!43422/nGGPVV||(if (--31455/Sv_g7W>=((17124)-dt5wnS)) then ((-17927)) else ((((!36445))))||41262<=(if 26185 then -47433 else -16365))*((33786)) then ((((if RF5ODN>=-15348 then -28465>=(-33187<=fTxFIU)--35920*41012<=(-iAyURe+-35969!=((Dh8Pc3)<-20036))>=19133+(5036) else zo0fZB)%!!8088&&(if (!nYpkYP) then -37356 else (tGAoob))%-10147!=p8poC4>=(-(31347!=-BlG2Tt)>=-498<hGmsR4%(if !-25549 then 30808 else BcXWnp)*--14036>-25720)*1656<(-QgUOch&&-489%(-5373)>(-(-25589)/-!25035+o6yns8))+JLsfPK-cvVeaW))) else -VE6wap-((qGJJt8)));
 *     let x6 = (if -(if 16677%(Jn0nuc)/rQERIs then --8780%(h1PhPw)<!-32953%!msJUkF else (if --nOGWgV+!44706 then -22628&&48781 else (if (iC7naK) then -z8gNXw else RledZL)))+(if ((-pF7gMV>(42082)))-LGoWSD>!-7490 then ugzzs2-HLbM4r<b_Guzm else -2956--12963-(if -4737 then (XWpgXg) else -tii04g))||anNtoX%!vUAXSN!=(if (if 20261 then 1353 else (Ti4TAc)) then 31683 else !Lz76Th/-11773)==(42761)<(!!(-4647&&wbli4S<=((--16129)-C8j0No)||imDHIp>-47124-(--348>=-48382))<-37710*((if iT4kdv then onxvW5 else A5tr73)-(aRqVJi)<=916%(-25153)==!AVr77M!=17283>(qVDTSb))--(oilKtn)<(xJHzKX)-W5LiR6/(if WUl8Xo<11512 then (if 14105 then Kzesua else Ueb61p) else !38980)/--46717) then (if (if (mob0Ie) then (-29006) else (-f_5tWo)) then !-jPdpfC+(if 44594 then -17541*fG4BsK<wmhUl9==z3Foau else --6444&&((-44359))==-rUnTJm)&&DYc20a||!--w_H9ac==(if (-35592) then !13236 else k_BESc)+47428>-10657==-jxNo6g else (if (if (if (-35337) then N4sm6F else -43626) then (--12599) else ((-vhNeS1))==(41110)) then sxmZEh else !46028)-!(WuDGLt)%(if 9960 then -FJgCrK else 20919)+-Tyttzt%-26424>--44683||-23344||!fIhkuh>=!d2VXBR<=nn8UdT<(-NabPfV!=45754)%!RNgS61>-19067==-18340<=-14935>zM9NGS<=(maTRQX)%-24214!=(if (((((if y5rKsN then mL_HwN else (-14067))||-28519)))>=(if --b44uw1==!O2V0mw then -yJsaCh==-39759 else ((HSxIst)))==(((r3F3gl)))*28632) then RomVJK else (25502)%(if -WTu6az-47139--35128>!TsSGKx then !(((TbJpNU)))+(((swsn6D)))>=(if 26837 then -28242 else !u9hvLZ) else -18505||q5xeyE!=SCEj3h+-GLFycv))) else (if ((if (ZaC7kO>(-43724))>=-21682 then (if (WFEISS<(hM6hGg)) then -(-29620)+(!Cl7C8J) else gjHA_n!=JUhVZw) else WjAjKX--43923/PbeD_6)||!42327/!-34099>-33152!=-LZJSC4-(YNLtXk)%(34771)%(if xgztXG then OA5WDk else !bP1PUC)%-(if (if YLgBV1 then (qnljpQ) else 9087)&&wtNLGb then -42229<MuU7km-(if (gvy3Go) then MEc98j else (l2E0QH)) else (if (if -45083 then zE8FD3 else (shY0_P)) then !49840 else (if RIvNkV then -20911 else 35027)))>(if -29039 then G5YW1x else -2567)&&--23731/(!-17395)+-46236*(Ju3R81))!=(if (if -qeKTFV>12279 then (!nu34iF<38244) else (tdSj_s>=((qo36Hy))))&&pCTKQI then (!vsYRt5) else 32612--31432-(if -40958 then -47483 else 16248)&&19765)-(if ---28731+--22726&&!amqQqq>=49980+-49417==-RzaQpg then (if (RT7hnk>24634)%-(NGHaWO)==-PLEKIH then m0GfHj*(rO6MhN)<(lbSYkb<!-6555) else (zrnoeG+-19884%-14580<(44214))) else 26151) then -24955 else xMFwTo));
 *     let x7 = -(if KVanUB then (gEYNuT*(--24574)*(Sfifhf<=(xErHbi))*(((JJJQb0)||UAz05t))==(r8FBn7)%37017) else (vndBOe)+!(if cC5l5O then RVTyK1 else NzNTLv)&&-20607!=(l_MUoC))-!4488*43284&&-tYhO6Q%-35353<-(if L2Mush then fQepni else NVabsp)>!AC5tNz/(45949<=(49284)!=(-39773)&&n4RRlG||(28337==-15404*-19302))-(--21624)>(rkc2F2)==!(TTbrrk)>=xMR2nJ-12114/15503>iRSWQu%e6QFDK-!-qG9Mmt==(-(if (10867) then (if 3506 then 22516 else 13148) else K0TeBu+!P_61yH)+(--23860*26793&&E7dhWR))!=DQyTJ9<=45796>=lYu_sm<40805>(if (zUSapr) then !49011----23338>-13601-(21619/L4eAS9)&&!tAfcpB%Rgw9Ty<=-2952 else (if (-12433||-47592)==(-48843)+-6600 then (26085>rkSYoj)%GwfyAJ!=((m66Q9b)) else 22026)---24125||!m143tO%(if (19923) then d24ACt else (if (if (if 18456<=zGSDia then (-136) else ((qZJaZL)*--29997)) then (if (if (-39335) then -48139 else 37845) then (!40+PrnzC_) else (if HrAHv9 then qzruRc else yYLhRc)) else -18045*IgTx4d==(-3303)>=!39445) then (-23688/sXEpQj<=(if hfNKSe then 36044 else 6061))*(if ---15383-(sAwmbF) then (R_YAm6)==((((-4806)))) else (OfU_Am)) else (if (-!48542-(-954)-!XLPrny+(205)) then !(-48263)!=27850==-35009 else (if -ZLinc6+(--10296) then (if (-xopzU3) then (31600) else !Bis21U) else (((22425)==YvbS1j)))))));
 *     let x8 = (if (if -49444*2079&&!l1DKTZ/(AGsv43) then -18119 else (-(!Tpt2l7)*-22262)%-16786) then (Fkwk9W--24613<fIWK8e<L35iEZ==((23211))<=Utv2K3*44090) else ((G16LNw>1966)/(BTmcZU)&&CtE4Ah)>!((t9Zb42)>!-49259)%-41231/(4088))<!!((((((((-18569))||22931)))))||-14619)&&((((if 23623 then ZPkF5u else y2E0Zq)||(28026%AOx7lP))))*--11608%a6dw1Q<(--39204==37616)!=(((ITCmXG)))!=-qBVBkq<=32657%-2096<((if -21317 then -527&&aQCXUx else --17294)%49324+hzQcF5<=34847>E2rMwM>21465%(-XrVeWO<=-17320)*(-(if -32230 then -B0r04h else !41879)>=(PPKtFo*!MxVrvN)))==(if !HZLt11>fveGau>JYdY5Z then (if 10886>X1N12A then 47639<Lg_xor else (if rMkQWS then ULfSy1 else LGxuee)) else --854/(if 40884 then (11694) else !10777))!=!!(RWbkyS)<=14086!=-7698*-(ZPy5qn-((ZOxQu6)))==-37967<(31504)>(if (if !L28PpZ<=(if -8541 then k3mkMj else (FZY74j&&(vRBw7Z))!=(if --7655 then !lf_GDz else 26258)) then ((-43111-uvFPW9/-AyYJTh>=(41019))<(if FwFdnv then qRtoFB else g0v4rP)>xzHn60*(if (if -ef8GV5*QRsBad then Tvf3oF<-19975 else !3171>=q70IsB) then -Ts_Zxu!=!xHhK5P>=-44198&&-em3ksH else q1kazf==MmGYa9)) else 7568) then ((--18306>((YK4GPc)*jtj_FP<!-fvCejR<=y88TLk)>=!-n8RlwY---45087||(UK5MPZ>-41581)&&555==P69szj)) else 26659||!-11595>cEWzOF%(if (8134) then -17743 else (sOB71r))||(Tjw0LQ&&!470||(if bIjFly then ZRNCWN else iTHpXO))-!(48325-PESH_p)>CfqMRt<(30529)&&-29199||-3380/-26545)+(-14193)<(-(if 38017&&-48937 then -8518&&17881 else !(j4Xhwx)*-28608)||((((-48993>(if QBQWIo then !20031 else G7LABp)))))&&(if eiEfRr then (10517)<(-13336) else (if Jt0VEm then !miysVs else eoe5m2))<=(--1488<=13592+(-vj936o>OaGJ6o))<-a9k4Nq>=-45053<-29065||(if -16396&&(gpwFwX) then -2463<22643 else -19590*Ib6SOG)>-16144==Hq7Usr);
 *     let x9 = (if (((!-BpR7xa/(if --38245 then 26660 else (if bqHV9J&&((-17535)) then (!uYHlv5+y4YmNf) else !(-14990)+-Ma7BSA))<((-CCzE1V<=LjLjOz*(FzQaxx)!=-!nqR7YO||RpZVwg==(-3678/((37485)))))>Hh8v61>(if (((!42609))) then --23160 else tl7cGQ)||!t5RgQq==ZuVgdk+(((-th_fdS%(k3zHDt))--(-BqkmCJ)>(GCU06R)))==41234&&((gmMf_w/-12230--umrC1u+((((f0R2bs))))*-VZnjUt/!I93PZV&&!GbvKKs<((if !oBvwxb then (-26897)<=(32722) else NiU2KU*38269)/(if (!yOCUca==(-lhEG_S)) then (WBUfwo)!=(-34956) else (if -28215 then MIH7Na else ((ejaTVv)))))))==(34577)))) then ((if (if (if !-21330--(if -4153 then h_nGN9 else (vDAyaZ))>(if hz0mKs then Axnpxu else -7701) then !cjN8Ex==(if !bTDKWO then !13947 else -14959)!=(!-34691*!9181) else -HQ606C>-44794>=ez7fFa*37936*(46025)||-9134) then -((35818))*(if AQ7MWX then (-11232) else DvtfwR)>=!yu06xh&&yUxluL&&-32195-(EoprLo%(-26154))>(BKQQyp)<=-44452 else (if -Iv6uzC then ((if -tpvKs0 then (-17948) else iu1Tld)/!QU8yQs%(((-j3Zi6b!=zOUMY6))==-A4fqv3*RnHfY4)) else (if (if 41916 then (-Z_pX89) else gPWY0f) then (25945)>=(DxGHKZ) else nBlmMl)+((TY62GW*43498&&(if !46431 then -44088 else (21303)))))) then cGippt else (if (!aoi1hp)&&15955<=-23932!=YNWBCU then (P8ApLp!=-37926||hd0ltO<=GKIER6) else (if -34947||--15941 then --23410 else -2423+(!17825)))==-16039&&-43966||((!-7667))<=(38588>=--8755<-41976<=Pfcumf)+!699!=(if 27465 then -44226 else BP573D)==!-38994!=MMj7Ot!=(31457/VzJSY0%CWK2hm%--28065)==-37508%37022%fntW3P)%(!!((if -31026-28699 then (!48896) else -rtAVOD*H1HFsv)%--20197||48434--23250%AGOwS3>=4996)<-(((if (if KBX7Dp then wrysFF else -7512) then ((!22945/--19110)) else KMiiKb>(26603))&&(if PcnUaw then -38297 else 43510)<UjO6jO*23393))&&(fBMcIX)+(6136>5388!=mCTAfb-wiqhiC*!(46306)==-24660)<=-1426<=(if huRIkE then 33201 else (-7801))*!-32875>=nx7Thn&&-32716>=(-8875)/mMy6bw)) else (if -10381!=(l1N92v)<=37375+-mIb1Rf+eDo9yW<47707+(qa_7u5-N5fw8I)>!K2shi7==-RrRWfB&&(!Dg4NIg)>(-9638) then (if (if (if k5uOCQ then -16621 else KZMrtl) then (lf7LNU)<=(27088) else !35528>-xiuP1g) then (!u0mRLS>=!dvFzic)/-upZFeW+p6Jb7J else (-19085%W4NTRG)%6267<=-42023)*-43510+(USY0oX||W5fl2Y)>-Z9lb4J<((oQLecQ))>-37842+-11278 else l3GubU)%(if (!-31388)<=!!CIhaZY%-47674==Ehxv64!=!-28513||-QtKOAb<=-25817!=(b1hB7i>(lgLUgA||-30379>-23880)) then (!FDpqLh/-804)%-7683/(-1620)>YB7Gt_*(((cX6HIZ)==-47076))||(OopX9h) else -!(if !-20887 then (pUt8Sw) else !LUh3B4)+((((!RJAZAG)>=8110)))>=((!NffpFI*Rrsq9R))<-(if -40386 then 35507 else -7179)/iOftbI&&(!dr_JiQ))%((!!(if (MbOvgT)>((zMMmUU==25456)) then --47995<=((RGKZu3))>(37545)&&!cQvA3i else (if ((!-16544))-(-9096) then -35593/(Z1Ktvm) else 14830/-11930))>=-2358==-(-22126)||-14293>=wyU9z1%XpJZoQ&&(!--49189+-13596||-GSHoXb%!WODze9&&(19757))%FMVSO1!=43716&&(if (Il2RLG>=xKLFoP!=-47155>Jqy01g) then ARULCN*pROqWc/OYsJh3>=43935 else (if -oANvH_ then (-40305) else 32606)%-34916)<(qqjN1w-22256/(if (28746) then Cuby7h else (nj_12r))*(H8fCKn)%I0EHJK==B7CDKX==e6HGqA)>=(FmEHAX)*-10909/956>=(FmI0K3))));";
 *  [%expect
 *   {| |}] *)
