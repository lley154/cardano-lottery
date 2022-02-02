
[nix-shell:~/src/cardano-lottery]$ cabal exec cardano-lottery-pab-sim
[INFO] Slot 0: TxnValidate 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a
[INFO] Starting plutus-starter PAB webserver on port 8080. Press enter to exit.
[INFO] Starting PAB backend server on port 9080
[INFO] ********* PAB Server is running *********
[INFO] Activate Init Contract then press return

[INFO] Initialising contract InitLottoContract with ID 769930fe-7fb7-48ce-baac-5624c131d1ed
[INFO] Activated instance 769930fe-7fb7-48ce-baac-5624c131d1ed on W872cb83
[INFO] adminPkh = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2
[INFO] adminPkhAddress = Address {addressCredential = PubKeyCredential a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, addressStakingCredential = Nothing}
[INFO] params : 
[INFO] (StartParams {spAdmin = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, spBenAddress = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, spDeadline = POSIXTime {getPOSIXTime = 1596064091999}, spTicket = 20000, spJackpot = 10000000},True)
[INFO] Lotto init contract wallet 1 (lotto admin)

[INFO] W872cb83: Balancing an unbalanced transaction:
                   Tx:
                     Tx da143e3b8aa55c7793ec29a717c66620e96d841c011d1a4bef26a5063216ef5c:
                       {inputs:
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",2000000)])]) addressed to
                           PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:}
                   Requires signatures:
                   Utxo index:
                   Validity range:
                     (-∞ , +∞)
[INFO] W872cb83: Finished balancing:
                   Tx 77dde17479dc647dbd945bc8a5adb1d14ce5cb2c6cde4244e53c28e31edc61f6:
                     {inputs:
                        - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!5

                     collateral inputs:
                       - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!5

                     outputs:
                       - Value (Map [(,Map [("",99997999990)])]) addressed to
                         PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                       - Value (Map [(,Map [("",2000000)])]) addressed to
                         PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",10)])])
                     mps:
                     signatures:
                       8d9de88fbf445b7f6c3875a14daba94caee2ffcbc9ac211c95aba0a2f5711853
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:}
[INFO] W872cb83: Balancing an unbalanced transaction:
                   Tx:
                     Tx 462485f93caaf0dac43f0c1b00f76bc4b7efff72833c22ce0061bcc8f4d9eb75:
                       {inputs:
                          - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!5

                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",10000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])])
                       fee: Value (Map [])
                       mps:
                         MintingPolicy { <script> }
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0",
                         [<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         "0">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         10000000,
                         0,
                         0,
                         0,
                         {"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194": 0}>>}
                   Requires signatures:
                   Utxo index:
                   Validity range:
                     (-∞ , +∞)
[INFO] W872cb83: Finished balancing:
                   Tx 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937:
                     {inputs:
                        - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!5

                     collateral inputs:
                       - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!5

                     outputs:
                       - Value (Map [(,Map [("",99989996809)])]) addressed to
                         PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                       - Value (Map [(,Map [("",10000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])])
                     fee: Value (Map [(,Map [("",3191)])])
                     mps:
                       MintingPolicy { <script> }
                     signatures:
                       8d9de88fbf445b7f6c3875a14daba94caee2ffcbc9ac211c95aba0a2f5711853
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0",
                       [<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       "0">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       10000000,
                       0,
                       0,
                       0,
                       {"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194": 0}>>}
[INFO] W872cb83: Submitting tx: 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937
[INFO] Slot 7: TxnValidate 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937
[INFO] 769930fe-7fb7-48ce-baac-5624c131d1ed: "lotto has been intialized Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}"
[INFO] Lotto found: Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}
[INFO] Initialising contract UseLottoContract with ID ec13d3f2-402e-48b3-9f18-2cb5abf4f080
[INFO] Activated instance ec13d3f2-402e-48b3-9f18-2cb5abf4f080 on W872cb83
[INFO] Initialising contract UseLottoContract with ID c7368403-28c3-41ed-8113-5186dba59b9e
[INFO] Activated instance c7368403-28c3-41ed-8113-5186dba59b9e on W7ce812d
[INFO] Initialising contract UseLottoContract with ID faf54a6d-c708-449b-b35c-bd94d46c3752
[INFO] Activated instance faf54a6d-c708-449b-b35c-bd94d46c3752 on Wc30efb7
[INFO] Initialising contract UseLottoContract with ID 6f35b842-585e-43fc-93b2-30375e78c936
[INFO] Activated instance 6f35b842-585e-43fc-93b2-30375e78c936 on W5f5a4f5
[INFO] start params : 
[INFO] (Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})},StartParams {spAdmin = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, spBenAddress = 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, spDeadline = POSIXTime {getPOSIXTime = 1596064091999}, spTicket = 20000, spJackpot = 10000000})
[INFO] Lotto start contract wallet 1 (lotto admin)

[INFO] ec13d3f2-402e-48b3-9f18-2cb5abf4f080: "setting lotto sequence to start of winning ticket number "
[INFO] W872cb83: Balancing an unbalanced transaction:
                   Tx:
                     Tx 18b99c3fa0aab7bd401d65eece5660360f8b48583af37ffd20068f03ea771d18:
                       {inputs:
                          - 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937!1
                            <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                            "U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                            1596064091999,
                            20000,
                            10000000>,
                            "0-">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",20000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         20000000,
                         1,
                         0,
                         0,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
                   Requires signatures:
                     a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2
                   Utxo index:
                     ( 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937!1
                     , - Value (Map [(,Map [("",10000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , +∞)
[INFO] W872cb83: Finished balancing:
                   Tx 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91:
                     {inputs:
                        - 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937!0

                        - 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937!1
                          <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                          "U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                          1596064091999,
                          20000,
                          10000000>,
                          "0-">
                     collateral inputs:
                       - 477c0c78a69fc05f806fade83b5a2225c8cb3a0418fbf60be11c7c52c212b937!0

                     outputs:
                       - Value (Map [(,Map [("",99979984800)])]) addressed to
                         PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                       - Value (Map [(,Map [("",20000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",12009)])])
                     mps:
                     signatures:
                       8d9de88fbf445b7f6c3875a14daba94caee2ffcbc9ac211c95aba0a2f5711853
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       20000000,
                       1,
                       0,
                       0,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
[INFO] W872cb83: Submitting tx: 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91
[INFO] Slot 16: TxnValidate 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91
[INFO] ec13d3f2-402e-48b3-9f18-2cb5abf4f080: "datum: Just (LottoDatum {admin = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, deadline = POSIXTime {getPOSIXTime = 1596064091999}, cost = 20000, winNum = \"0\", winners = [(a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2,\"0\")], mph = 1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f, jackpot = 10000000, seqNum = 0, treasury = 0, adminFees = 0, beneficiaries = Map {unMap = [(a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2,0)]}})"
[INFO] ec13d3f2-402e-48b3-9f18-2cb5abf4f080: "lotto has started Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}"
[INFO] buy params : 
[INFO] (Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})},123)
[INFO] Lotto buy 123 contract wallet 2 (lotto player)

[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "appending lotto sequence to lotto ticket: "
[WARNING] c7368403-28c3-41ed-8113-5186dba59b9e: "Plutus.Contract.StateMachine.runStep: Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
[INFO] W7ce812d: Balancing an unbalanced transaction:
                   Tx:
                     Tx b871590bf48fed48806a5753ed4d98b01670132ea46b4c2e3cf537743679b16b:
                       {inputs:
                          - 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91!1
                            <"0-123",
                            "\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",22000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                         - Value (Map [(,Map [("",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-123",1)])]) addressed to
                           PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                       mint: Value (Map [(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-123",1)])])
                       fee: Value (Map [])
                       mps:
                         MintingPolicy { <script> }
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         20980000,
                         1,
                         980000,
                         40000,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
                   Requires signatures:
                   Utxo index:
                     ( 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91!1
                     , - Value (Map [(,Map [("",20000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , POSIXTime 1596064091999 ]
[INFO] W7ce812d: Finished balancing:
                   Tx ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd:
                     {inputs:
                        - 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91!1
                          <"0-123",
                          "\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167">
                        - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!2

                     collateral inputs:
                       - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!2

                     outputs:
                       - Value (Map [(,Map [("",99993985229)])]) addressed to
                         PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                       - Value (Map [(,Map [("",2000000)])]) addressed to
                         PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                       - Value (Map [(,Map [("",22000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       - Value (Map [(,Map [("",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-123",1)])]) addressed to
                         PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                     mint: Value (Map [(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-123",1)])])
                     fee: Value (Map [(,Map [("",14771)])])
                     mps:
                       MintingPolicy { <script> }
                     signatures:
                       98c77c40ccc536e0d433874dae97d4a0787b10b3bca0dc2e1bdc7be0a544f0ac
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 5000})) True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       20980000,
                       1,
                       980000,
                       40000,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
[INFO] W7ce812d: Submitting tx: ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd
[INFO] Slot 36: TxnValidate ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd
[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "datum: Just (LottoDatum {admin = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, deadline = POSIXTime {getPOSIXTime = 1596064091999}, cost = 20000, winNum = \"0-\", winners = [(557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d,\"0-\")], mph = 1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f, jackpot = 20000000, seqNum = 1, treasury = 0, adminFees = 0, beneficiaries = Map {unMap = [(557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d,0)]}})"
[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "utxos: fromList [(TxOutRef {txOutRefId = ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",99993985229)])])}),(TxOutRef {txOutRefId = ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd, txOutRefIdx = 1},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",2000000)])])}),(TxOutRef {txOutRefId = ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd, txOutRefIdx = 3},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [(\"0-123\",1)])])})]"
[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "value: Value (Map [(b97d49a80b916d377e9f67bfdd78410d96660c3fc9701556613c0588,Map [(\"123\",1)])])"
[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "lookups: ScriptLookups {slMPS = fromList [(b97d49a80b916d377e9f67bfdd78410d96660c3fc9701556613c0588,MintingPolicy { <script> })], slTxOutputs = fromList [], slOtherScripts = fromList [], slOtherData = fromList [], slPaymentPubKeyHashes = fromList [], slTypedValidator = Nothing, slOwnPaymentPubKeyHash = Nothing, slOwnStakePubKeyHash = Nothing}"
[INFO] buy params : 
[INFO] (Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})},789)
[INFO] Lotto buy 789 contract wallet 3 (lotto player)

[INFO] faf54a6d-c708-449b-b35c-bd94d46c3752: "appending lotto sequence to lotto ticket: "
[WARNING] faf54a6d-c708-449b-b35c-bd94d46c3752: "Plutus.Contract.StateMachine.runStep: Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
[INFO] Wc30efb7: Balancing an unbalanced transaction:
                   Tx:
                     Tx 06bb8399d7c2d13d878f4c1b3d3594a1c5c61c4160ca2dbd199aced6f1f37a93:
                       {inputs:
                          - ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!2
                            <"0-789",
                            ".\n\214\f2\a$\140\236\212}\189\227\215R\224\170\209A\214\184\248\SUB\194\198\236\162|">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                         - Value (Map [(,Map [("",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-789",1)])]) addressed to
                           PubKeyCredential: 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c (no staking credential)
                       mint: Value (Map [(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-789",1)])])
                       fee: Value (Map [])
                       mps:
                         MintingPolicy { <script> }
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         21960000,
                         1,
                         1960000,
                         80000,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
                   Requires signatures:
                   Utxo index:
                     ( ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!2
                     , - Value (Map [(,Map [("",22000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , POSIXTime 1596064091999 ]
[INFO] Wc30efb7: Finished balancing:
                   Tx 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a:
                     {inputs:
                        - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!0

                        - ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!2
                          <"0-789",
                          ".\n\214\f2\a$\140\236\212}\189\227\215R\224\170\209A\214\184\248\SUB\194\198\236\162|">
                     collateral inputs:
                       - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!0

                     outputs:
                       - Value (Map [(,Map [("",99993985229)])]) addressed to
                         PubKeyCredential: 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c (no staking credential)
                       - Value (Map [(,Map [("",2000000)])]) addressed to
                         PubKeyCredential: 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c (no staking credential)
                       - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       - Value (Map [(,Map [("",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-789",1)])]) addressed to
                         PubKeyCredential: 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c (no staking credential)
                     mint: Value (Map [(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-789",1)])])
                     fee: Value (Map [(,Map [("",14771)])])
                     mps:
                       MintingPolicy { <script> }
                     signatures:
                       4cdc632449cde98d811f78ad2e2d15a278731bc5e3b8821739a47534c324fe20
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 5000})) True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       21960000,
                       1,
                       1960000,
                       80000,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
[INFO] Wc30efb7: Submitting tx: 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a
[INFO] Slot 51: TxnValidate 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a
[INFO] faf54a6d-c708-449b-b35c-bd94d46c3752: "datum: Just (LottoDatum {admin = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, deadline = POSIXTime {getPOSIXTime = 1596064091999}, cost = 20000, winNum = \"0-\", winners = [(557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d,\"0-\")], mph = 1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f, jackpot = 20980000, seqNum = 1, treasury = 980000, adminFees = 40000, beneficiaries = Map {unMap = [(557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d,0)]}})"
[INFO] faf54a6d-c708-449b-b35c-bd94d46c3752: "utxos: fromList [(TxOutRef {txOutRefId = 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",99993985229)])])}),(TxOutRef {txOutRefId = 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a, txOutRefIdx = 1},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",2000000)])])}),(TxOutRef {txOutRefId = 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a, txOutRefIdx = 3},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [(\"0-789\",1)])])})]"
[INFO] faf54a6d-c708-449b-b35c-bd94d46c3752: "value: Value (Map [(f3363259d5a559dc9bd850f5c0754d1f99705b3377d805870ebdfa4b,Map [(\"789\",1)])])"
[INFO] faf54a6d-c708-449b-b35c-bd94d46c3752: "lookups: ScriptLookups {slMPS = fromList [(f3363259d5a559dc9bd850f5c0754d1f99705b3377d805870ebdfa4b,MintingPolicy { <script> })], slTxOutputs = fromList [], slOtherScripts = fromList [], slOtherData = fromList [], slPaymentPubKeyHashes = fromList [], slTypedValidator = Nothing, slOwnPaymentPubKeyHash = Nothing, slOwnStakePubKeyHash = Nothing}"
[INFO] close params : 
[INFO] (Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})},123)
[INFO] Lotto close 123 contract wallet 1 (lotto admin)

[INFO] ec13d3f2-402e-48b3-9f18-2cb5abf4f080: "appending lotto sequence to lotto ticket: "
[INFO] W872cb83: Balancing an unbalanced transaction:
                   Tx:
                     Tx b8600cbb02a89a6f6f5d50facd4dd2e67b2c74e62f3f9a1de1d26aa5f2f4183b:
                       {inputs:
                          - 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a!2
                            <"0-123">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-123",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         21960000,
                         1,
                         1960000,
                         80000,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
                   Requires signatures:
                     a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2
                   Utxo index:
                     ( 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a!2
                     , - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , +∞)
[INFO] W872cb83: Finished balancing:
                   Tx 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d:
                     {inputs:
                        - 5f7cb3896a17700af985a971be9667ae5060108acabd446753c302a3e780191a!2
                          <"0-123">
                        - 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91!0

                     collateral inputs:
                       - 69746899bb4d8ff2f169fe7fda5ad52daad3161da16a90b406284597dabbfa91!0

                     outputs:
                       - Value (Map [(,Map [("",99979972791)])]) addressed to
                         PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                       - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",12009)])])
                     mps:
                     signatures:
                       8d9de88fbf445b7f6c3875a14daba94caee2ffcbc9ac211c95aba0a2f5711853
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-123",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       21960000,
                       1,
                       1960000,
                       80000,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
[INFO] W872cb83: Submitting tx: 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d
[INFO] Slot 56: TxnValidate 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d
[INFO] ec13d3f2-402e-48b3-9f18-2cb5abf4f080: "datum: Just (LottoDatum {admin = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, deadline = POSIXTime {getPOSIXTime = 1596064091999}, cost = 20000, winNum = \"0-\", winners = [(557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d,\"0-\")], mph = 1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f, jackpot = 21960000, seqNum = 1, treasury = 1960000, adminFees = 80000, beneficiaries = Map {unMap = [(557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d,0)]}})"
[INFO] ec13d3f2-402e-48b3-9f18-2cb5abf4f080: "utxos: fromList [(TxOutRef {txOutRefId = 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",99979972791)])])})]"
[INFO] redeem params : 
[INFO] Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}
[INFO] Lotto redeem contract wallet 2 (lotto player)

[WARNING] c7368403-28c3-41ed-8113-5186dba59b9e: "Plutus.Contract.StateMachine.runStep: Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
[INFO] W7ce812d: Balancing an unbalanced transaction:
                   Tx:
                     Tx d0a2ac197d8910e69c2c4aff2d3b1c9c2be85729fcbc32967276039510beafef:
                       {inputs:
                          - 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d!1
                            <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-123",1)])]) addressed to
                           PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                         - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-123",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">,
                         <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                         "0-123">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         21960000,
                         1,
                         1960000,
                         80000,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
                   Requires signatures:
                   Utxo index:
                     ( 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d!1
                     , - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , POSIXTime 1596064091999 ]
[INFO] W7ce812d: Finished balancing:
                   Tx f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859:
                     {inputs:
                        - 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d!1
                          <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167">
                        - ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!0

                        - ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!1

                        - ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!3

                     collateral inputs:
                       - ccbab2d35f4a691012ea7cb12ad6e6e29e733173112db74ade0ed2ad1f0fdabd!0

                     outputs:
                       - Value (Map [(,Map [("",99995973220)])]) addressed to
                         PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                       - Value (Map [(,Map [("",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [("0-123",1)])]) addressed to
                         PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                       - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",12009)])])
                     mps:
                     signatures:
                       98c77c40ccc536e0d433874dae97d4a0787b10b3bca0dc2e1bdc7be0a544f0ac
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 5000})) True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-123",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">,
                       <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                       "0-123">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       21960000,
                       1,
                       1960000,
                       80000,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 0}>>}
[INFO] W7ce812d: Submitting tx: f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859
[INFO] Slot 62: TxnValidate f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859
[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "utxos: fromList [(TxOutRef {txOutRefId = f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",99995973220)])])}),(TxOutRef {txOutRefId = f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859, txOutRefIdx = 1},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [(\"0-123\",1)])])})]"
[INFO] calc params : 
[INFO] Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}
[INFO] Lotto calc-payout contract wallet 1 (lotto admin)

[INFO] W872cb83: Balancing an unbalanced transaction:
                   Tx:
                     Tx b245777657fdacd2dca82bda4e337acc6cd0a2605f478d25963031ea00fdb610:
                       {inputs:
                          - f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859!2
                            <>
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-123",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">,
                         <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                         "0-123">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         21960000,
                         1,
                         1960000,
                         80000,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 10980000,
                         "\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167": 10980000}>>}
                   Requires signatures:
                     a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2
                   Utxo index:
                     ( f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859!2
                     , - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , +∞)
[INFO] W872cb83: Finished balancing:
                   Tx 1dc390132b8ec552e0b4f3ee8b1437e377a96e8a45992adab88626bb14de3e97:
                     {inputs:
                        - 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d!0

                        - f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859!2
                          <>
                     collateral inputs:
                       - 16e6a0cde50718b33aa133ef01c9249534e331cb8b89f7c908925f9372c80d5d!0

                     outputs:
                       - Value (Map [(,Map [("",99979960782)])]) addressed to
                         PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                       - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",12009)])])
                     mps:
                     signatures:
                       8d9de88fbf445b7f6c3875a14daba94caee2ffcbc9ac211c95aba0a2f5711853
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-123",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">,
                       <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                       "0-123">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       21960000,
                       1,
                       1960000,
                       80000,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 10980000,
                       "\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167": 10980000}>>}
[INFO] W872cb83: Submitting tx: 1dc390132b8ec552e0b4f3ee8b1437e377a96e8a45992adab88626bb14de3e97
[INFO] Slot 67: TxnValidate 1dc390132b8ec552e0b4f3ee8b1437e377a96e8a45992adab88626bb14de3e97
[INFO] payout params : 
[INFO] Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}
[INFO] Lotto payout contract wallet 2 (lotto player)

[INFO] W7ce812d: Balancing an unbalanced transaction:
                   Tx:
                     Tx bca3bee762d5a45a204ee0b88bbdcc0c96d0addbedea286587725edc8b00351d:
                       {inputs:
                          - 1dc390132b8ec552e0b4f3ee8b1437e377a96e8a45992adab88626bb14de3e97!1
                            <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",13020000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-123",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">,
                         <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                         "0-123">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         10980000,
                         1,
                         1960000,
                         80000,
                         {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 10980000}>>}
                   Requires signatures:
                     80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7
                   Utxo index:
                     ( 1dc390132b8ec552e0b4f3ee8b1437e377a96e8a45992adab88626bb14de3e97!1
                     , - Value (Map [(,Map [("",24000000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , +∞)
[INFO] W7ce812d: Finished balancing:
                   Tx 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595:
                     {inputs:
                        - 1dc390132b8ec552e0b4f3ee8b1437e377a96e8a45992adab88626bb14de3e97!1
                          <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167">
                     collateral inputs:
                       - f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859!0

                     outputs:
                       - Value (Map [(,Map [("",10967991)])]) addressed to
                         PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                       - Value (Map [(,Map [("",13020000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",12009)])])
                     mps:
                     signatures:
                       98c77c40ccc536e0d433874dae97d4a0787b10b3bca0dc2e1bdc7be0a544f0ac
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-123",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">,
                       <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                       "0-123">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       10980000,
                       1,
                       1960000,
                       80000,
                       {"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =": 10980000}>>}
[INFO] W7ce812d: Submitting tx: 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595
[INFO] Slot 73: TxnValidate 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595
[INFO] c7368403-28c3-41ed-8113-5186dba59b9e: "utxos: fromList [(TxOutRef {txOutRefId = 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",10967991)])])}),(TxOutRef {txOutRefId = f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",99995973220)])])}),(TxOutRef {txOutRefId = f0cc23eefd6f93965a52ae4cad18ed6669919b8d425bce9b30a19721ec385859, txOutRefIdx = 1},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",2000000)]),(1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f,Map [(\"0-123\",1)])])})]"
[INFO] payout params : 
[INFO] Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 5}, ttCurrencySymbol = ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887})}
[INFO] Lotto payout contract wallet 4 (sponsor)

[INFO] W5f5a4f5: Balancing an unbalanced transaction:
                   Tx:
                     Tx d245e93ba04b4e79ab5ab5023dde0282abc7c2f796f0b87c22cdc4d22aa26ea1:
                       {inputs:
                          - 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595!1
                            <"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =">
                       collateral inputs:
                       outputs:
                         - Value (Map [(,Map [("",2040000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                           ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                       mint: Value (Map [])
                       fee: Value (Map [])
                       mps:
                       signatures:
                       validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                       data:
                         <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                         1596064091999,
                         20000,
                         "0-123",
                         [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                         "0-">,
                         <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                         "0-123">],
                         "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                         0,
                         1,
                         1960000,
                         80000,
                         {}>>}
                   Requires signatures:
                     557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d
                   Utxo index:
                     ( 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595!1
                     , - Value (Map [(,Map [("",13020000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 )
                   Validity range:
                     (-∞ , +∞)
[INFO] W5f5a4f5: Finished balancing:
                   Tx c1a2920fcadc33414b328042409cddbeec6272f542681a1fa7d5aa5a0ec0ec99:
                     {inputs:
                        - 21b465c984902f93bfcfbe2bc3f6abc776e49ac8b1a2d8f104cc173d356e4595!1
                          <"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =">
                     collateral inputs:
                       - 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a!1

                     outputs:
                       - Value (Map [(,Map [("",10967991)])]) addressed to
                         PubKeyCredential: 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d (no staking credential)
                       - Value (Map [(,Map [("",2040000)]),(ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887,Map [(0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475,1)])]) addressed to
                         ScriptCredential: 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475 (no staking credential)
                     mint: Value (Map [])
                     fee: Value (Map [(,Map [("",12009)])])
                     mps:
                     signatures:
                       c0a4b02f44c212ba6c1197df5a5cf8bd1a3dceef9c42e4a17c0d48f8ae468cdb
                     validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                     data:
                       <<"\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194",
                       1596064091999,
                       20000,
                       "0-123",
                       [<"U}#\192\165\&3\180\210\149\172-\193Kx:~\252);\194>\222\136\166\254\253 =",
                       "0-">,
                       <"\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167",
                       "0-123">],
                       "\ESC\161i\236\SOR'\CAN\129\231R\202\156\188\218\248`\DEL\203\&6\239\244\224R\aQ\153?",
                       0,
                       1,
                       1960000,
                       80000,
                       {}>>}
[INFO] W5f5a4f5: Submitting tx: c1a2920fcadc33414b328042409cddbeec6272f542681a1fa7d5aa5a0ec0ec99
[INFO] Slot 79: TxnValidate c1a2920fcadc33414b328042409cddbeec6272f542681a1fa7d5aa5a0ec0ec99
[INFO] 6f35b842-585e-43fc-93b2-30375e78c936: "utxos: fromList [(TxOutRef {txOutRefId = 8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a, txOutRefIdx = 1},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",100000000000)])])}),(TxOutRef {txOutRefId = c1a2920fcadc33414b328042409cddbeec6272f542681a1fa7d5aa5a0ec0ec99, txOutRefIdx = 0},PublicKeyChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = PubKeyCredential 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, addressStakingCredential = Nothing}, _ciTxOutValue = Value (Map [(,Map [(\"\",10967991)])])})]"

[INFO] Balances at the end of the simulation
[INFO] Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3: 
webserver: shutting down
[INFO]     {, ""}: 100000000000
[INFO] Wallet 3a4778247ad35117d7c3150d194da389f3148f4a: 
[INFO]     {, ""}: 100000000000
[INFO] Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648: 
[INFO]     {, ""}: 100000000000
[INFO] Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8: 
[INFO]     {, ""}: 100010967991
[INFO] Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
[INFO]     {1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f, "0-123"}: 1
[INFO]     {, ""}: 100008941211
[INFO] Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
[INFO]     {, ""}: 99979960782
[INFO] Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f: 
[INFO]     {, ""}: 100000000000
[INFO] Wallet c19599f22890ced15c6a87222302109e83b78bdf: 
[INFO]     {, ""}: 100000000000
[INFO] Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154: 
[INFO]     {1ba169ec0e52271881e752ca9cbcdaf8607fcb36eff4e0520751993f, "0-789"}: 1
[INFO]     {, ""}: 99997985229
[INFO] Wallet d3eddd0d37989746b029a0e050386bc425363901: 
[INFO]     {, ""}: 100000000000
[INFO] Script 25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475: 
[INFO]     {, ""}: 2040000
[INFO]     {ac085a84688af91ddc991a8fd30a03dce1e5e428146de5fadbfdd887, 0x25999223af6d1154d7b6538c3e62fb707669b64c89a582d301954475}: 1



nix-shell:~/src/cardano-lottery/scripts]$ cabal run cardano-lottery-scripts -- ./tmp scripts --unapplied-validators
Up to date
Writing scripts (unapplied) to: ./tmp
Writing script: ./tmp/cardano-lottery-test-1-unapplied.flat (Size: 2.5kB, Cost: ExCPU 245405256, ExMemory 585456)
Writing script: ./tmp/cardano-lottery-test-2-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1210722592, ExMemory 3617426)
Writing script: ./tmp/cardano-lottery-test-3-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1695580961, ExMemory 5191004)
Writing script: ./tmp/cardano-lottery-test-4-unapplied.flat (Size: 2.2kB, Cost: ExCPU 325757454, ExMemory 755280)
Writing script: ./tmp/cardano-lottery-test-5-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1695580961, ExMemory 5191004)
Writing script: ./tmp/cardano-lottery-test-6-unapplied.flat (Size: 2.2kB, Cost: ExCPU 325757454, ExMemory 755280)
Writing script: ./tmp/cardano-lottery-test-7-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1098159101, ExMemory 3263864)
Writing script: ./tmp/cardano-lottery-test-8-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1777118021, ExMemory 5400028)
Writing script: ./tmp/cardano-lottery-test-9-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1254850804, ExMemory 3764091)
Writing script: ./tmp/cardano-lottery-test-10-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1144430837, ExMemory 3460284)
Writing script: ./tmp/cardano-lottery-test-11-unapplied.flat (Size: 9.6kB, Cost: ExCPU 1127904562, ExMemory 3412392)
Total Size: 83.7kB, Cost: ExCPU 11901268003, ExMemory 35396109

/home/lawrence/src/plutus-apps/dist-newstyle/src/plutus-3b55de0732f6b166/plutus-core/plutus-core/src/PlutusCore/Evaluation/Machine/ExBudget.hs



[nix-shell:~/Downloads/plutus-apps/plutus-pab-executables]$ cabal build plutus-pab-examples

nix-shell:~/Downloads/plutus-apps/plutus-pab-executables]$ cabal exec plutus-pab-examples -- migrate --config plutus-pab.yaml.sample

[nix-shell:~/Downloads/plutus-apps/plutus-pab-executables]$ cabal exec plutus-pab-examples -- all-servers --config plutus-pab.yaml.sample


export DIR=~/Downloads/cardano-wallet

export SHELLEY_TEST_DATA=~/src/plutus-apps/plutus-pab/local-cluster/cluster-data/cardano-node-shelley

$ cabal exec plutus-pab-examples -- all-servers --config plutus-pab.yaml.sample



[pab:Info:13] [2022-01-22 14:19:20.75 UTC] Starting PAB Server on port 9082
Starting: MockWallet
Started: MockWallet
[pab:Info:15] [2022-01-22 14:19:20.75 UTC] Processing chain event TxnValidate 09604d638153f5182cb588fc3d9779a9e0962964e9110e8eae561d88819b554b
Starting: PABWebserver
[pab:Info:25] [2022-01-22 14:19:20.82 UTC] Starting wallet server on port 9081
Started: PABWebserver
[pab:Info:59] [2022-01-22 14:19:20.91 UTC] Restoring PAB state ...
[pab:Info:59] [2022-01-22 14:19:20.91 UTC] No contract instance were restored in the PAB state.
[pab:Info:59] [2022-01-22 14:19:20.91 UTC] Starting PAB backend server on port 9080
Starting: ChainIndex
Started: ChainIndex
[pab:Info:116] [2022-01-22 14:19:20.91 UTC] Starting node client thread
[pab:Info:116] [2022-01-22 14:19:20.91 UTC] Starting chain index on port 9083


export WALLET_ID_1=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`
export WALLET_ID_2=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`

# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWalletId": '$WALLET_ID_1'}}' \
  http://localhost:9080/api/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWalletId": '$WALLET_ID_2'}}' \
  http://localhost:9080/api/contract/activate | jq

$ echo $WALLET_ID_1
"969f6a9871439d9071c053619cba9891ec3f0452"
lawrence@lawrence-MacBookAir:~$ echo $WALLET_ID_2
"ddf1a2a5806f356d8f881c73eb6ff5e0b592349f"

curl -H "Content-Type: application/json" -v \
       -X POST \
       -d '{"caID":{"tag":"IntegrationTest"},"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
       localhost:9080/api/contract/activate

    


{
  "caID": {
    "tag": "UniswapInit"
  },
  "caWallet": {
    "getWalletId": {
      "unWalletId": "string"
    }
  }
}

ads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"60013552-8797-4975-9e7b-c8a812807d12"}


{
    "caID": {
        "tag": "UniswapInit"
    },
    "caWallet": {
        "getWalletId": "969f6a9871439d9071c053619cba9891ec3f0452"
    }
}


[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- migrate --config cardano-lottery-pab.yaml

[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- all-servers --config cardano-lottery-pab.yaml
Starting: StartNode
Started: StartNode
Starting: MockWallet
[pab:Info:10] [2022-01-22 16:36:08.45 UTC] Starting slot coordination thread. Initial slot time: 2020-07-29T21:44:51Z Slot length: 1000ms
[pab:Info:10] [2022-01-22 16:36:08.45 UTC] Starting PAB Server on port 9082
Started: MockWallet
[pab:Info:12] [2022-01-22 16:36:08.48 UTC] Processing chain event TxnValidate 09604d638153f5182cb588fc3d9779a9e0962964e9110e8eae561d88819b554b
Starting: PABWebserver
[pab:Info:25] [2022-01-22 16:36:08.49 UTC] Starting wallet server on port 9081
Started: PABWebserver
[pab:Info:41] [2022-01-22 16:36:08.57 UTC] Restoring PAB state ...
[pab:Info:41] [2022-01-22 16:36:08.57 UTC] No contract instance were restored in the PAB state.
[pab:Info:41] [2022-01-22 16:36:08.57 UTC] Starting PAB backend server on port 9080
Starting: ChainIndex
Started: ChainIndex
[pab:Info:87] [2022-01-22 16:36:08.57 UTC] Starting node client thread
[pab:Info:87] [2022-01-22 16:36:08.57 UTC] Starting chain index on port 9083

lawrence@lawrence-MacBookAir:~$ curl -s http://localhost:9080/api/contract/definitions | jq


    ],
    "csrDefinition": {
      "tag": "InitLottoContract"
    }


lawrence@lawrence-MacBookAir:~$ export WALLET_ID_1=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`
lawrence@lawrence-MacBookAir:~$ echo $WALLET_ID_1
"2ae8ea1050ae679390f6e8946962a4e534007174"
lawrence@lawrence-MacBookAir:~$ export WALLET_ID_2=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`
lawrence@lawrence-MacBookAir:~$ echo $WALLET_ID_2
"7bc049579c6db571b7e1ad4d37a100e81a16f464"

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b"}lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 

Current block: 3. Current slot: 46810964
[pab:Info:148] [2022-01-22 16:47:36.33 UTC] Initialising contract InitLottoContract with ID a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b
[pab:Info:148] [2022-01-22 16:47:36.40 UTC] Activated instance a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b on W2ae8ea1
Current block: 3. Current slot: 46810965

export INSTANCE_ID=a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq
{
  "cicCurrentState": {
    "observableState": null,
    "logs": [],
    "hooks": [
      {
        "rqID": 1,
        "itID": 1,
        "rqRequest": {
          "aeMetadata": null,
          "aeDescription": {
            "getEndpointDescription": "init"
          }
        }
      }
    ],
    "lastLogs": [],
    "err": null
  },
  "cicYieldedExportTxs": [],
  "cicContract": {
    "unContractInstanceId": "a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b"
  },
  "cicStatus": "Active",
  "cicDefinition": {
    "tag": "InitLottoContract"
  },
  "cicWallet": {
    "getWalletId": "2ae8ea1050ae679390f6e8946962a4e534007174"
  }
}




        sp = StartParams
                { spAdmin          = pkh
                , spBenAddress     = pkh
                , spJackpot        = jackpot'
                , spTicket         = ticket'
                , spDeadline       = deadline'
                }


export INSTANCE_ID=c753ff28-f998-4680-9fa5-d3fac466b87d


[INFO] sp parms: 
[INFO] "{\"spAdmin\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spBenAddress\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spJackpot\":10000000,\"spTicket\":20000,\"spDeadline\":1596064091999}"
[INFO] useTT : 
[INFO] "true"


[INFO] params : 
[INFO] "[{\"spAdmin\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spBenAddress\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spJackpot\":10000000,\"spTicket\":20000,\"spDeadline\":1596064091999},true]"


######################################## Jan 22 ##################################


[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- migrate --config cardano-lottery-pab.yaml

[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- all-servers --config cardano-lottery-pab.yaml
Starting: StartNode
Started: StartNode
[pab:Info:10] [2022-01-23 17:28:19.97 UTC] Starting slot coordination thread. Initial slot time: 2020-07-29T21:44:51Z Slot length: 1000ms
Starting: MockWallet
Started: MockWallet
[pab:Info:10] [2022-01-23 17:28:19.98 UTC] Starting PAB Server on port 9082
[pab:Info:12] [2022-01-23 17:28:20.00 UTC] Processing chain event TxnValidate 09604d638153f5182cb588fc3d9779a9e0962964e9110e8eae561d88819b554b
Starting: PABWebserver
[pab:Info:26] [2022-01-23 17:28:20.02 UTC] Starting wallet server on port 9081
Started: PABWebserver
[pab:Info:41] [2022-01-23 17:28:20.10 UTC] Restoring PAB state ...
[pab:Info:41] [2022-01-23 17:28:20.10 UTC] No contract instance were restored in the PAB state.
[pab:Info:41] [2022-01-23 17:28:20.10 UTC] Starting PAB backend server on port 9080
Starting: ChainIndex
Started: ChainIndex
[pab:Info:88] [2022-01-23 17:28:20.10 UTC] Starting node client thread
[pab:Info:88] [2022-01-23 17:28:20.10 UTC] Starting chain index on port 9083




[nix-shell:~/Downloads]$ curl -s -d '' http://localhost:9081/create
{"wiWallet":{"getWalletId":"bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"},"wiPaymentPubKeyHash":{"unPaymentPubKeyHash":{"getPubKeyHash":"4484759c2c3fbc9c9c2704901b010b3b1c51bca942413b299bbe0cf3"}}}

[nix-shell:~/Downloads]$ export WALLET_ID_1='"bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"'

[nix-shell:~/Downloads]$ echo $WALLET_ID_1
"bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"



[nix-shell:~/Downloads]$ curl -s -d '' http://localhost:9081/create
{"wiWallet":{"getWalletId":"da8f13c8f9d13215e77d7c37aa4444b888cedb7e"},"wiPaymentPubKeyHash":{"unPaymentPubKeyHash":{"getPubKeyHash":"d42e8cbb1f44177a1109ca624e94bdfcfdd13d9170ce9d5c0c815710"}}}
[nix-shell:~/Downloads]$ export WALLET_ID_2='"da8f13c8f9d13215e77d7c37aa4444b888cedb7e"'

[nix-shell:~/Downloads]$ echo $WALLET_ID_2
"da8f13c8f9d13215e77d7c37aa4444b888cedb7e"


[nix-shell:~/Downloads/cardano-lottery/app]$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"7c6bb925-af3e-4934-95c1-e30cbb5317f6"}

[pab:Info:182] [2022-01-23 17:43:01.27 UTC] Initialising contract InitLottoContract with ID 7c6bb925-af3e-4934-95c1-e30cbb5317f6
[pab:Info:182] [2022-01-23 17:43:01.33 UTC] Activated instance 7c6bb925-af3e-4934-95c1-e30cbb5317f6 on Wbdf5e8b

[nix-shell:~/Downloads/cardano-lottery/app]$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
[]


[pab:Error:184] [2022-01-23 17:51:00.86 UTC] 7c6bb925-af3e-4934-95c1-e30cbb5317f6: "OtherError \"Error in $: parsing (a, b) failed, expected Array, but encountered Object\""

[nix-shell:~/Downloads/cardano-lottery/app]$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq
{
  "cicCurrentState": {
    "observableState": null,
    "logs": [
      {
        "_logMessageContent": "OtherError \"Error in $: parsing (a, b) failed, expected Array, but encountered Object\"",
        "_logLevel": "Error"
      }
    ],
    "hooks": [
      {
        "rqID": 1,
        "itID": 2,
        "rqRequest": {
          "aeMetadata": null,
          "aeDescription": {
            "getEndpointDescription": "init"
          }
        }
      }
    ],
    "lastLogs": [
      {
        "_logMessageContent": "OtherError \"Error in $: parsing (a, b) failed, expected Array, but encountered Object\"",
        "_logLevel": "Error"
      }
    ],
    "err": null
  },
  "cicYieldedExportTxs": [],
  "cicContract": {
    "unContractInstanceId": "7c6bb925-af3e-4934-95c1-e30cbb5317f6"
  },
  "cicStatus": "Active",
  "cicDefinition": {
    "tag": "InitLottoContract"
  },
  "cicWallet": {
    "getWalletId": "bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"
  }
}


lot: 46902873
[pab:Error:184] [2022-01-23 18:19:24.98 UTC] 7c6bb925-af3e-4934-95c1-e30cbb5317f6: "OtherError \"Error in $[0].spAdmin: parsing Ledger.Address.PaymentPubKeyHash(PaymentPubKeyHash) failed, key \\\"unPaymentPubKeyHash\\\" not found\""
Current block: 5. Current slot: 46902874


Current block: 5. Current slot: 46903571
Wallet.Emulator.Wallet.walletPubKey: Wallet Wallet bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382 is not a mock wallet
CallStack (from HasCallStack):
  error, called at src/Wallet/Emulator/Wallet.hs:144:22 in plutus-contract-0.1.0.0-2f17c7bdd61646ab0c6fe498f5ce48773511927703d743a2d774b3ad5e1ff438:Wallet.Emulator.Wallet
Current block: 5. Current slot: 46903572



############################# JAN 23 #####################################

The tip of the local node: SlotNo 180
Connecting to the node using socket: /run/user/1000/test-cluster192560/node/node.socket
Starting webserver on port 9083
A Swagger UI for the endpoints are available at http://localhost:9083/swagger/swagger-ui


[pab:Info:1500] [2022-01-23 20:15:05.36 UTC] Restoring PAB state ...
[pab:Info:1500] [2022-01-23 20:15:05.36 UTC] No contract instance were restored in the PAB state.
[pab:Info:1500] [2022-01-23 20:15:05.36 UTC] Starting PAB backend server on port 9080

Restored wallet: WalletId {getWalletId = 2d4cc31a4b3116ab86bfe529d30d9c362acd0b44}
Passphrase: cardano-wallet


[cluster:Notice:1384] [2022-01-23 20:14:47.91 UTC] {"string":"Wallet url: http://127.0.0.1:46493/, EKG url: none, Prometheus url:none"}


curl -s -d '' http://localhost:46493/create



http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys

http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/1852H


lawrence-MacBookAir:~$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/1852
"addr_vk1hd5qqfrktfqr29fy0jmz2vnj2fxey7avvul4j7nyjqykv8sxsrms7szx9a"

lawrence@lawrence-MacBookAir:~$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/1852?hash=true
"addr_vkh1eq0mkwyqm7muef2sjcw0vun5rhk7u56d8w0ua5u0xu92xeqam9m"lawrence@lawrence-MacBookAir:~$ 


curl http://127.0.0.1:46493/v2/shared-wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys?format=extended


[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/utxo
{"entries":[{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]}]}


[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/addresses[{"derivation_path":["1852H","1815H","0H","0","0"],"id":"addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x","state":"used"},{"derivation_path":["1852H","1815H","0H","0","1"],"id":"addr1qxvy2tutkjmrs7gyehmgeq6qnzhjjys98kweanhp3c69r2ufl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqgk0wgx","state":"used"},{"derivation_path":["1852H","1815H","0H","0","2"],"id":"addr1qy39m0dz3pg0l6unlpmn6pwt3tagp0qkfl0kxlq5wk8h76yfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqjnm7j8","state":"used"},{"derivation_path":["1852H","1815H","0H","0","3"],"id":"addr1q9l9xu3h6q692glfmlfz76rrkj63y33qv0xs0kfsdkl4x8yfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqtw4aa5","state":"used"}



[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/addresses/addr1q9l9xu3h6q692glfmlfz76rrkj63y33qv0xs0kfsdkl4x8yfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqtw4aa5
{"address_style":"Shelley","spending_key_hash_bech32":"addr_vkh10efhyd7sx32j86wl6ghksca5k5fyvgrre5rajvrdhaf3cpdjw73","network_tag":1,"stake_key_hash_bech32":"stake_vkh1387xr5saml9af4pk22lst3qvx3h609y8zs3mv5zj6as5c62hmun","stake_reference":"by value","spending_key_hash":"7e537237d0345523e9dfd22f6863b4b512462063cd07d9306dbf531c","stake_key_hash":"89fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7614c"}


curl http://127.0.0.1:46493/v2/addresses/addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x

[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/addresses/addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x
{"address_style":"Shelley","spending_key_hash_bech32":"addr_vkh1nmta3qgf5g8upllg9ynqgrc8dznt68d7e6upyj5s2rlc2x3fpx5","network_tag":1,"stake_key_hash_bech32":"stake_vkh1387xr5saml9af4pk22lst3qvx3h609y8zs3mv5zj6as5c62hmun","stake_reference":"by value","spending_key_hash":"9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85","stake_key_hash":"89fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7614c"}




lawrence@lawrence-MacBookAir:~$ curl http://localhost:46493/v2/wallets/
[{"address_pool_gap":20,"passphrase":{"last_updated_at":"2022-01-23T22:13:40.087168596Z"},"balance":{"available":{"unit":"lovelace","quantity":1000000000000},"total":{"unit":"lovelace","quantity":1000000000000},"reward":{"unit":"lovelace","quantity":0}},"id":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44","state":{"status":"ready"},"name":"plutus-wallet","assets":{"available":[],"total":[]},"tip":{"height":{"unit":"block","quantity":5270},"slot_number":66,"absolute_slot_number":10766,"epoch_number":107,"time":"2022-01-23T22:48:56.2Z"},"delegation":{"next":[],"active":{"status":"not_delegating"}}}]lawrence@lawrence-MacBookAir:~$ 


curl http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44


[nix-shell:~/Downloads/cardano-lottery/app]$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"b4b2703f-31d3-4edc-9e89-df86d2fb4c5d"}

export INSTANCE_ID=b4b2703f-31d3-4edc-9e89-df86d2fb4c5d


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq


curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init


############################### Jan 24 #################################


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request POST \
>   --url http://localhost:46493/v2/wallets \
>   --header 'Content-Type: application/json' \
>   --data '{
>     "name": "test_cf_1",
>     "mnemonic_sentence": ["shift", "badge", "heavy", "action", "tube", "divide", "course", "quality", "capable", "velvet", "cart", "marriage", "vague", "aware", "maximum", "exist", "crime", "file", "analyst", "great", "cabbage", "course", "sad", "apology"],
>     "passphrase": "test123456"
> }'
{"address_pool_gap":20,"passphrase":{"last_updated_at":"2022-01-24T16:02:51.141839196Z"},"balance":{"available":{"unit":"lovelace","quantity":0},"total":{"unit":"lovelace","quantity":0},"reward":{"unit":"lovelace","quantity":0}},"id":"5076b34c6949dbd150eb9c39039037543946bdce","state":{"status":"syncing","progress":{"unit":"percent","quantity":0}},"name":"test_cf_1","assets":{"available":[],"total":[]},"tip":{"height":{"unit":"block","quantity":0},"slot_number":0,"absolute_slot_number":0,"epoch_number":0,"time":"2022-01-24T01:05:51Z"},"delegation":{"next":[],"active":{"status":"not_delegating"}}}l

awrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request GET \
>   --url 'http://localhost:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses?state=unused'
[{"derivation_path":["1852H","1815H","0H","0","0"],"id":"addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","1"],"id":"addr1q9g2eglv9gf2rksvdj53t6ajfgzkycaadlt2fatjyn4etpze0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qepwhvl","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","2"],"id":"addr1q9fk9lkc7qnh94ta84w7g8wexzg8k84m5rcj8hzpez58ntje0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qk8k7ld","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","3"],"id":"addr1q9cyzrh222u9udjvdm2a6dmsyar5qqasqzyksv6038gzvy2e0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00quvf760","state":"unused"}, ...

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request POST \
>   --url http://localhost:46493/v2/wallets \
>   --header 'Content-Type: application/json' \
>   --data '{
>     "name": "test_wallet_1",
>     "mnemonic_sentence": ["shift", "badge", "heavy", "action", "tube", "divide", "course", "quality", "capable", "velvet", "cart", "marriage", "vague", "aware", "maximum", "exist", "crime", "file", "analyst", "great", "cabbage", "course", "sad", "apology"],
>     "passphrase": "test123456"
> }'
{"address_pool_gap":20,"passphrase":{"last_updated_at":"2022-01-24T17:05:29.755656006Z"},"balance":{"available":{"unit":"lovelace","quantity":0},"total":{"unit":"lovelace","quantity":0},"reward":{"unit":"lovelace","quantity":0}},"id":"5076b34c6949dbd150eb9c39039037543946bdce","state":{"status":"syncing","progress":{"unit":"percent","quantity":0}},"name":"test_wallet_1","assets":{"available":[],"total":[]},"tip":{"height":{"unit":"block","quantity":0},"slot_number":0,"absolute_slot_number":0,"epoch_number":0,"time":"2022-01-24T17:02:55Z"},"delegation":{"next":[],"active":{"status":"not_delegating"}}}


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request POST \
>    --url http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions \
>    --header 'Content-Type: application/json' \
>    --data '{
>         "passphrase": "cardano-wallet",
>         "payments": [
>             {
>             "address": "addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7",
>             "amount": {
>                 "quantity": 1000000000,
>                 "unit": "lovelace"
>                 }
>             }
>         ]
> }'
{"outputs":[{"amount":{"unit":"lovelace","quantity":1000000000},"address":"addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7","assets":[]},{"amount":{"unit":"lovelace","quantity":98999869000},"address":"addr1qydz7tcs8wy4m0nn3zkvnnqslyxuftd98arvsswj43zxxpufl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqnt8cj6","assets":[]}],"status":"pending","amount":{"unit":"lovelace","quantity":1000131000},"mint":[],"fee":{"unit":"lovelace","quantity":131000},"deposit":{"unit":"lovelace","quantity":0},"id":"114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2","direction":"outgoing","metadata":null,"script_validity":"valid","pending_since":{"height":{"unit":"block","quantity":1524},"slot_number":59,"absolute_slot_number":3159,"epoch_number":31,"time":"2022-01-24T17:13:26.8Z"},"withdrawals":[],"collateral":[],"inputs":[{"amount":{"unit":"lovelace","quantity":100000000000},"address":"addr1v9fvyz26pnc7jftnezn784my73y8r8ylmhunl6j8mypdddgtqhp4x","id":"d3e4ff652ffb1bdda69bd60ca331cf2444cd49149255f1aad50564593c830bbf","index":4,"assets":[]}],"expires_at":{"slot_number":61,"absolute_slot_number":39161,"epoch_number":391,"time":"2022-01-24T19:13:27.


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl http://localhost:46493/v2/wallets/ | jq
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  2365    0  2365    0     0   230k      0 --:--:-- --:--:-- --:--:--  256k
[
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:03:32.045280301Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 996999607000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 996999607000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "2d4cc31a4b3116ab86bfe529d30d9c362acd0b44",
    "state": {
      "status": "ready"
    },
    "name": "plutus-wallet",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  },
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:05:29.755656006Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "5076b34c6949dbd150eb9c39039037543946bdce",
    "state": {
      "status": "ready"
    },
    "name": "test_wallet_1",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  },
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:06:01.597936395Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "f6b3948d73f5e317ac130419ada6047262bfbb22",
    "state": {
      "status": "ready"
    },
    "name": "test_wallet_2",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  },
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:06:22.797119755Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "00d3b1fc7af1236ee7f6226a83ed2ad360b91013",
    "state": {
      "status": "ready"
    },
    "name": "test_wallet_3",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  }
]



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl http://127.0.0.1:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses
[{"derivation_path":["1852H","1815H","0H","0","0"],"id":"addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7","state":"used"},{"derivation_path":["1852H","1815H","0H","0","1"],"id":"addr1q9g2eglv9gf2rksvdj53t6ajfgzkycaadlt2fatjyn4etpze0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qepwhvl","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","2"],"id":"addr1q9fk9lkc7qnh94ta84w7g8wexzg8k84m5rcj8hzpez58ntje0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qk8k7ld","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","3"],"id":"addr1q9cyzrh222u9udjvdm2a6dmsyar5qqasqzyksv6038gzvy2e0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00quvf760","state":"unused"},

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl http://127.0.0.1:46493/v2/addresses/addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7 | jq
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   405    0   405    0     0   395k      0 --:--:-- --:--:-- --:--:--  395k
{
  "address_style": "Shelley",
  "spending_key_hash_bech32": "addr_vkh1jfgygyk82w4359m5jt32x23vuj7axs6hte69f0l3fjzavprjvsq",
  "network_tag": 1,
  "stake_key_hash_bech32": "stake_vkh1t97s4t4qq9c05rkgefkr32q6t2hnxrxjz5rndgk9mnpau9fwk40",
  "stake_reference": "by value",
  "spending_key_hash": "92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d6",
  "stake_key_hash": "597d0aaea00170fa0ec8ca6c38a81a5aaf330cd2150736a2c5dcc3de"
}
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 




lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"4323d3dd-0552-44f7-b098-4862e0db6e74"}

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ export INSTANCE_ID=4323d3dd-0552-44f7-b098-4862e0db6e74
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ echo $INSTANCE_ID
4323d3dd-0552-44f7-b098-4862e0db6e74
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init


[pab:Warning:3177] [2022-01-24 17:36:52.77 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-sign\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 403, statusMessage = \"Forbidden\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Mon, 24 Jan 2022 17:36:52 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The given encryption passphrase doesn't match the one I use to encrypt the root private key of the given wallet: 5076b34c6949dbd150eb9c39039037543946bdce\\\",\\\"code\\\":\\\"wrong_encryption_passphrase\\\"}\"})"


cardano-wallet.api-server:Info:3966] [2022-01-24 17:36:52.70 UTC] {"string":"[RequestId 21] POST /v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-balance 202 Accepted in 0.069304522s"}
[cardano-wallet.api-server:Info:3966] [2022-01-24 17:36:52.75 UTC] {"string":"[RequestId 22] [POST] /v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-sign"}
[cardano-wallet.api-server:Info:3966] [2022-01-24 17:36:52.77 UTC] {"string":"[RequestId 22] POST /v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-sign 403 Forbidden in 0.019715701s"}



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
[]lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq
{
  "cicCurrentState": {
    "observableState": null,
    "logs": [
      {
        "_logMessageContent": {
          "mkTxLogResult": {
            "Right": {
              "unBalancedTxTx": {
                "txData": [
                  [
                    "47e742b2b5c960e8cb8dcb7a76110e73864f9ec31abcd3d7c550586d15fce8d5",
                    "d8799fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d61b000001739cd54f5f194e2041309fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d64130ffff581cecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae1a00989680000000a1581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d600ffff"
                  ]
                ],
                "txInputs": [
                  {
                    "txInType": {
                      "tag": "ConsumePublicKeyAddress"
                    },
                    "txInRef": {
                      "txOutRefId": {
                        "getTxId": "114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2"
                      },
                      "txOutRefIdx": 0
                    }
                  }
                ],
                "txRedeemers": [
                  [
                    [
                      "Mint",
                      0
                    ],
                    "d8799f581c72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5d87980ff"
                  ]
                ],
                "txOutputs": [
                  {
                    "txOutValue": {
                      "getValue": [
                        [
                          {
                            "unCurrencySymbol": ""
                          },
                          [
                            [
                              {
                                "unTokenName": ""
                              },
                              10000000
                            ]
                          ]
                        ],
                        [
                          {
                            "unCurrencySymbol": "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e"
                          },
                          [
                            [
                              {
                                "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                              },
                              1
                            ]
                          ]
                        ]
                      ]
                    },
                    "txOutAddress": {
                      "addressStakingCredential": null,
                      "addressCredential": {
                        "contents": "72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5",
                        "tag": "ScriptCredential"
                      }
                    },
                    "txOutDatumHash": "47e742b2b5c960e8cb8dcb7a76110e73864f9ec31abcd3d7c550586d15fce8d5"
                  }
                ],
                "txValidRange": {
                  "ivTo": [
                    {
                      "tag": "PosInf"
                    },
                    true
                  ],
                  "ivFrom": [
                    {
                      "tag": "NegInf"
                    },
                    true
                  ]
                },
                "txMint": {
                  "getValue": [
                    [
                      {
                        "unCurrencySymbol": "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e"
                      },
                      [
                        [
                          {
                            "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                          },
                          1
                        ]
                      ]
                    ]
                  ]
                },
                "txFee": {
                  "getValue": []
                },
                "txCollateral": [],
                "txMintScripts": [
                  {
                    "getMintingPolicy": "590a9901000..."
                  }
                ],
                "txSignatures": []
              },
              "unBalancedTxRequiredSignatories": [],
              "unBalancedTxValidityTimeRange": {
                "ivTo": [
                  {
                    "tag": "PosInf"
                  },
                  true
                ],
                "ivFrom": [
                  {
                    "tag": "NegInf"
                  },
                  true
                ]
              },
              "unBalancedTxUtxoIndex": []
            }
          },
          "mkTxLogTxConstraints": {
            "txOwnInputs": [],
            "txOwnOutputs": [
              {
                "ocValue": {
                  "getValue": [
                    [
                      {
                        "unCurrencySymbol": ""
                      },
                      [
                        [
                          {
                            "unTokenName": ""
                          },
                          10000000
                        ]
                      ]
                    ],
                    [
                      {
                        "unCurrencySymbol": "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e"
                      },
                      [
                        [
                          {
                            "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                          },
                          1
                        ]
                      ]
                    ]
                  ]
                },
                "ocDatum": "d8799fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d61b000001739cd54f5f194e2041309fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d64130ffff581cecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae1a00989680000000a1581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d600ffff"
              }
            ],
            "txConstraints": [
              {
                "contents": "d8799fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d61b000001739cd54f5f194e2041309fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d64130ffff581cecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae1a00989680000000a1581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d600ffff",
                "tag": "MustIncludeDatum"
              },
              {
                "contents": [
                  "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e",
                  "d8799f581c72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5d87980ff",
                  {
                    "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                  },
                  1
                ],
                "tag": "MustMintValue"
              },
              {
                "contents": {
                  "txOutRefId": {
                    "getTxId": "114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2"
                  },
                  "txOutRefIdx": 0
                },
                "tag": "MustSpendPubKeyOutput"
              }
            ]
          },
          "mkTxLogLookups": {
            "slOtherScripts": [],
            "slPaymentPubKeyHashes": [],
            "slOwnPaymentPubKeyHash": null,
            "slOwnStakePubKeyHash": null,
            "slOtherData": [],
            "slTypedValidator": {
              "tvForwardingMPSHash": "ecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae",
              "tvForwardingMPS": {
                "getMintingPolicy": "59092701000033233..."
              },
              "tvValidator": {
                "getValidator": "5927ac0100003323322..."
              },
              "tvValidatorHash": "72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
            },
            "slTxOutputs": [
              [
                {
                  "txOutRefId": {
                    "getTxId": "114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2"
                  },
                  "txOutRefIdx": 0
                },
                {
                  "_ciTxOutAddress": {
                    "addressStakingCredential": {
                      "contents": {
                        "contents": {
                          "getPubKeyHash": "597d0aaea00170fa0ec8ca6c38a81a5aaf330cd2150736a2c5dcc3de"
                        },
                        "tag": "PubKeyCredential"
                      },
                      "tag": "StakingHash"
                    },
                    "addressCredential": {
                      "contents": {
                        "getPubKeyHash": "92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d6"
                      },
                      "tag": "PubKeyCredential"
                    }
                  },
                  "_ciTxOutValue": {
                    "getValue": [
                      [
                        {
                          "unCurrencySymbol": ""
                        },
                        [
                          [
                            {
                              "unTokenName": ""
                            },
                            1000000000
                          ]
                        ]
                      ]
                    ]
                  },
                  "tag": "PublicKeyChainIndexTxOut"
                }
              ]
            ],
            "slMPS": [
              [
                "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e",
                {
                  "getMintingPolicy": "590a990100003..."
                }
              ],
              [
                "ecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae",
                {
                  "getMintingPolicy": "59092701000...
                }
              ]
            ]
          }
        },
        "_logLevel": "Debug"
      }
    ],
    "hooks": [],
    "lastLogs": [],
    "err": null
  },
  "cicYieldedExportTxs": [],
  "cicContract": {
    "unContractInstanceId": "4323d3dd-0552-44f7-b098-4862e0db6e74"
  },
  "cicStatus": "Active",
  "cicDefinition": {
    "tag": "InitLottoContract"
  },
  "cicWallet": {
    "getWalletId": "5076b34c6949dbd150eb9c39039037543946bdce"
  }
}



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
EndpointCallError (EndpointNotAvailable (ContractInstanceId {unContractInstanceId = fc05158c-c660-43a4-bcb9-c797d4ca604a}) (EndpointDescription {getEndpointDescription = "init"}))


[pab:Warning:10018] [2022-01-24 19:27:03.40 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 400, statusMessage = \"Bad Request\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Mon, 24 Jan 2022 19:27:03 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"I was unable to assign execution units to one of your redeemers: minting(f9fdc049adaa45b1043d31193dceb6127b31a8394cd0133f3231ef53); Its execution is failing with the following error: ValidationFailedV1 (CekError An error has occurred:  User error: The provided Plutus code called 'error'.) [\\\\\\\"S8\\\\\\\",\\\\\\\"PT5\\\\\\\"].\\\",\\\"code\\\":\\\"redeemer_script_failure\\\"}\"})"



[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.08 UTC] {"string":"[RequestId 32] [GET] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0?hash=true"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.08 UTC] {"string":"[RequestId 32] GET /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0 200 OK in 0.002541562s"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.12 UTC] {"string":"[RequestId 33] [POST] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance"}
[cardano-wallet.wallet-engine:Info:10934] [2022-01-24 19:27:03.12 UTC] {"string":"2d4cc31a: Selection report (summarized):\nSelectionReportSummarized:\n  computedFee: 0.131700\n  adaBalanceOfSelectedInputs: 20.000000\n  adaBalanceOfExtraCoinSource: 0.000000\n  adaBalanceOfExtraCoinSink: 0.000000\n  adaBalanceOfRequestedOutputs: 2.000000\n  adaBalanceOfGeneratedChangeOutputs: 17.868300\n  numberOfSelectedInputs: 1\n  numberOfSelectedCollateralInputs: 0\n  numberOfRequestedOutputs: 1\n  numberOfGeneratedChangeOutputs: 1\n  numberOfUniqueNonAdaAssetsInSelectedInputs: 0\n  numberOfUniqueNonAdaAssetsInRequestedOutputs: 0\n  numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs: 0\n"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.12 UTC] {"string":"[RequestId 33] POST /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance 202 Accepted in 0.005562668s"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.17 UTC] {"string":"[RequestId 34] [GET] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0?hash=true"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.17 UTC] {"string":"[RequestId 34] GET /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0 200 OK in 0.000887215s"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.38 UTC] {"string":"[RequestId 35] [POST] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance"}
[cardano-wallet.wallet-engine:Info:10934] [2022-01-24 19:27:03.39 UTC] {"string":"2d4cc31a: Selection report (summarized):\nSelectionReportSummarized:\n  computedFee: 1.491600\n  adaBalanceOfSelectedInputs: 100000.000000\n  adaBalanceOfExtraCoinSource: 0.000000\n  adaBalanceOfExtraCoinSink: 0.000000\n  adaBalanceOfRequestedOutputs: 10.000000\n  adaBalanceOfGeneratedChangeOutputs: 99988.508400\n  numberOfSelectedInputs: 1\n  numberOfSelectedCollateralInputs: 1\n  numberOfRequestedOutputs: 1\n  numberOfGeneratedChangeOutputs: 1\n  numberOfUniqueNonAdaAssetsInSelectedInputs: 0\n  numberOfUniqueNonAdaAssetsInRequestedOutputs: 1\n  numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs: 0\n"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.40 UTC] {"string":"[RequestId 35] POST /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance 400 Bad Request in 0.01885629s"}



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
EndpointCallError (EndpointNotAvailable (ContractInstanceId {unContractInstanceId = fc05158c-c660-43a4-bcb9-c797d4ca604a}) (EndpointDescription {getEndpointDescription = "init"}))



[cardano-wallet.wallet-engine:Error:10369] [2022-01-26 21:25:24.01 UTC] {"string":"Transaction 53f0c705 failed: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (WrongNetwork Mainnet (fromList [Addr Testnet (ScriptHashObj (ScriptHash \"870e8957cd9567e515cad1e5d91d5f7688421c6a34a30c3fad7a99ba\")) StakeRefNull]))))])) AlonzoEraInCardanoMode"}
[cardano-wallet.api-server:Error:10369] [2022-01-26 21:25:24.02 UTC] {"string":"[RequestId 17] POST /v2/proxy/transactions 500 Internal Server Error in 0.017427987s"}


[pab:Info:399] [2022-01-26 21:19:07.66 UTC] Initialising contract InitLottoContract with ID 704ee014-8819-4207-96c4-ed56298312e3
[pab:Info:399] [2022-01-26 21:19:07.71 UTC] Activated instance 704ee014-8819-4207-96c4-ed56298312e3 on Wb6ac58e
[pab:Warning:401] [2022-01-26 21:25:24.02 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"localhost\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/proxy/transactions\"), requestQueryString = fromList [], requestBody = Just ((),application/octet-stream), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Wed, 26 Jan 2022 21:25:23 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The submitted transaction was rejected by the local node. Here's an error message that may help with debugging: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (WrongNetwork Mainnet (fromList [Addr Testnet (ScriptHashObj (ScriptHash \\\\\\\"870e8957cd9567e515cad1e5d91d5f7688421c6a34a30c3fad7a99ba\\\\\\\")) StakeRefNull]))))])) AlonzoEraInCardanoMode\\\",\\\"code\\\":\\\"created_invalid_transaction\\\"}\"})"


pab:Warning:1918] [2022-01-26 21:49:12.81 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-sign\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 403, statusMessage = \"Forbidden\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Wed, 26 Jan 2022 21:49:12 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The given encryption passphrase doesn't match the one I use to encrypt the root private key of the given wallet: b6ac58e44f232e1fd863b7da0520b3c99d18bab5\\\",\\\"code\\\":\\\"wrong_encryption_passphrase\\\"}\"})"



##################################### JAN 26 ########################################33
pab:Info:2077] [2022-01-26 22:07:40.93 UTC] Initialising contract InitLottoContract with ID 89180b7e-d84a-41a7-a5c4-09a04a6dac2c
[pab:Info:2077] [2022-01-26 22:07:40.93 UTC] Activated instance 89180b7e-d84a-41a7-a5c4-09a04a6dac2c on Wb6ac58e
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1480}, tipBlockId = 86d57ff039b1cdd7dcfeeb760a4a7683bfefdf37ad0ade7ddc6ef737582b3dbf, tipBlockNo = BlockNumber {unBlockNumber = 668}}, targetPoint = Point {pointSlot = Slot {getSlot = 2085}, pointBlockId = f884df88dda0bd736c19c6f2c439763671cb48ccd0b7e09919bda86452e2eea6}})
[pab:Info:2079] [2022-01-26 22:10:56.86 UTC] 89180b7e-d84a-41a7-a5c4-09a04a6dac2c: "lotto has been intialized Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 4db08b8c449cf658469eded2414924ddab35fa38599006f0fe308711b5050c98, txOutRefIdx = 0}, ttCurrencySymbol = 25b3ec827aa736b7a98d25d40092b421810eb27407939acf01c0024e})}"
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 2170}, tipBlockId = 1404b16bf526dd94a93213bf4c7bdf2b8a698096e2d1f84f499b6caa66fcf602, tipBlockNo = BlockNumber {unBlockNumber = 1031}}, targetPoint = Point {pointSlot = Slot {getSlot = 2883}, pointBlockId = dec2200cbea540de58b693f205d3cc1cabe8d0fa374009230ebb8a58ec770efe}


curl -s http://localhost:9080/api/contract/definitions | jq


[nix-shell:~/Downloads/cardano-lottery/app]$ curl -s http://localhost:9080/api/contract/definitions | jq
[
  {
    "csrSchemas": [
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "spAdmin",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spBenAddress",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spDeadline",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spTicket",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spJackpot",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "tag": "FormSchemaBool"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "init"
        }
      }
    ],
    "csrDefinition": "InitLottoContract"
  },
  {
    "csrSchemas": [
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "lToken",
                  {
                    "contents": {
                      "contents": [
                        [
                          "ttOutRef",
                          {
                            "contents": [
                              [
                                "txOutRefId",
                                {
                                  "contents": [
                                    [
                                      "getTxId",
                                      {
                                        "tag": "FormSchemaString"
                                      }
                                    ]
                                  ],
                                  "tag": "FormSchemaObject"
                                }
                              ],
                              [
                                "txOutRefIdx",
                                {
                                  "tag": "FormSchemaInteger"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ],
                        [
                          "ttCurrencySymbol",
                          {
                            "contents": [
                              [
                                "unCurrencySymbol",
                                {
                                  "tag": "FormSchemaString"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ]
                      ],
                      "tag": "FormSchemaObject"
                    },
                    "tag": "FormSchemaMaybe"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "tag": "FormSchemaInteger"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "buy"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "calc-payout"
        }
      },
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "lToken",
                  {
                    "contents": {
                      "contents": [
                        [
                          "ttOutRef",
                          {
                            "contents": [
                              [
                                "txOutRefId",
                                {
                                  "contents": [
                                    [
                                      "getTxId",
                                      {
                                        "tag": "FormSchemaString"
                                      }
                                    ]
                                  ],
                                  "tag": "FormSchemaObject"
                                }
                              ],
                              [
                                "txOutRefIdx",
                                {
                                  "tag": "FormSchemaInteger"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ],
                        [
                          "ttCurrencySymbol",
                          {
                            "contents": [
                              [
                                "unCurrencySymbol",
                                {
                                  "tag": "FormSchemaString"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ]
                      ],
                      "tag": "FormSchemaObject"
                    },
                    "tag": "FormSchemaMaybe"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "tag": "FormSchemaInteger"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "close"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "collect"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "payout"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "redeem"
        }
      },
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "lToken",
                  {
                    "contents": {
                      "contents": [
                        [
                          "ttOutRef",
                          {
                            "contents": [
                              [
                                "txOutRefId",
                                {
                                  "contents": [
                                    [
                                      "getTxId",
                                      {
                                        "tag": "FormSchemaString"
                                      }
                                    ]
                                  ],
                                  "tag": "FormSchemaObject"
                                }
                              ],
                              [
                                "txOutRefIdx",
                                {
                                  "tag": "FormSchemaInteger"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ],
                        [
                          "ttCurrencySymbol",
                          {
                            "contents": [
                              [
                                "unCurrencySymbol",
                                {
                                  "tag": "FormSchemaString"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ]
                      ],
                      "tag": "FormSchemaObject"
                    },
                    "tag": "FormSchemaMaybe"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "contents": [
                [
                  "spAdmin",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spBenAddress",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spDeadline",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spTicket",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spJackpot",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "start"
        }
      }
    ],
    "csrDefinition": "UseLottoContract"
  }
]

[pab:Info:1768] [2022-01-27 14:59:34.04 UTC] 7655417b-eb56-48c0-913f-56e235296dc5: "lotto has been intialized Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = ede42dfb0526a4acc97088958648fdf56aed125cd633860aca977e3d2f193ff3, txOutRefIdx = 0}, ttCurrencySymbol = a291967c430b0a3a63b19463ae815f59c31b38685de69c704dcf5976})}"
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 1426}, pointBlockId = c9beab0836f0d37498501472aaf145093a1a9cf274ba854402338dbfdd3f4f88}})
[pab:Info:2051] [2022-01-27 15:00:38.96 UTC] Initialising contract UseLottoContract with ID 4be96adf-f991-4fa2-ba0a-986217842056
[pab:Info:2051] [2022-01-27 15:00:38.96 UTC] Activated instance 4be96adf-f991-4fa2-ba0a-986217842056 on Wb6ac58e
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 1924}, pointBlockId = 904ad009892f4db9de19c491b960ca9fc17f1c8181b73ddb171c8291d67abd88}})
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 2757}, pointBlockId = 90bfa028ac296359359d84ff2ba4579694df66eaa150af059349cab15a7d203f}})
[pab:Info:2054] [2022-01-27 15:04:25.80 UTC] 4be96adf-f991-4fa2-ba0a-986217842056: "setting lotto sequence to start of winning ticket number "
[cardano-wallet.api-server:Error:2304] [2022-01-27 15:04:25.94 UTC] {"string":"[RequestId 11] POST /v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-balance 500 Internal Server Error in 0.053316004s"}
[pab:Warning:2054] [2022-01-27 15:04:25.94 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-balance\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Thu, 27 Jan 2022 15:04:25 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"What was supposed to be an initial overestimation of fees turned out to be an underestimation, and I cannot recover. This is a cardano-wallet bug.\\\",\\\"code\\\":\\\"created_invalid_transaction\\\"}\"})"
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 4048}, pointBlockId = 80621bed6744bd709359e77a5569f2b3d52429365a0fd405d6665b9a0973576c}})


bash-4.4# cd /usr/
bash-4.4# ls
local
bash-4.4# cd local/
bash-4.4# ls
bin
bash-4.4# cd bin/
bash-4.4# ls
cardano-cli  cardano-node  entrypoint  run-client  run-network	run-node
bash-4.4# ls -l
total 28
lrwxrwxrwx 1 0 0   94 Jan  1  1980 cardano-cli -> /nix/store/iks84bw5djycl9zwlvmqcs4i53a1kbpc-cardano-cli-exe-cardano-cli-1.33.0/bin/cardano-cli
lrwxrwxrwx 1 0 0   97 Jan  1  1980 cardano-node -> /nix/store/aginrkjcnlzbxsvla5bd8lnx73a475yi-cardano-node-exe-cardano-node-1.33.0/bin/cardano-node
-r-xr-xr-x 1 0 0  318 Jan  1  1980 entrypoint
-r-xr-xr-x 1 0 0  200 Jan  1  1980 run-client
-r-xr-xr-x 1 0 0 1120 Jan  1  1980 run-network
-r-xr-xr-x 1 0 0 5186 Jan  1  1980 run-node
bash-4.4# vi run-network 
bash: vi: command not found
bash-4.4# cat run-network 
#!/nix/store/xvvgw9sb8wk6d2c0j3ybn7sll67s3s4z-bash-4.4-p23/bin/bash
if [[ -z "$NETWORK" ]]; then
  echo "[Error] Cannot obtain NETWORK env variable"
  exit 1
elif [[ "$NETWORK" == "alonzo-purple" ]]; then
  exec /nix/store/0zx2cmsz9awkgmzkcq8bhbjb1yf56mpp-cardano-node-alonzo-purple/bin/cardano-node-alonzo-purple $@

elif [[ "$NETWORK" == "mainnet" ]]; then
  exec /nix/store/h7b31hirspc9pdz6azzhqbpch3x0qqc3-cardano-node-mainnet/bin/cardano-node-mainnet $@

elif [[ "$NETWORK" == "p2p" ]]; then
  exec /nix/store/pgg0m3ihkx3b5gs6qw759fi7bj2lfy70-cardano-node-p2p/bin/cardano-node-p2p $@

elif [[ "$NETWORK" == "shelley_qa" ]]; then
  exec /nix/store/lhyhp42c99h718v6q7c186arjf8jgib4-cardano-node-shelley_qa/bin/cardano-node-shelley_qa $@

elif [[ "$NETWORK" == "staging" ]]; then
  exec /nix/store/5k4yamn1npby7iabbwkn4axxl7ipsnkf-cardano-node-staging/bin/cardano-node-staging $@

elif [[ "$NETWORK" == "testnet" ]]; then
  exec /nix/store/65vm8y6g5mrfyjdg8kasx2b0r90h144d-cardano-node-testnet/bin/cardano-node-testnet $@

else
  echo "[Error] Managed configuration for network "$NETWORK" does not exist"
  exit 1
fi

bash-4.4# cat /nix/store/65vm8y6g5mrfyjdg8kasx2b0r90h144d-cardano-node-testnet/bin/cardano-node-testnet
#!/nix/store/xvvgw9sb8wk6d2c0j3ybn7sll67s3s4z-bash-4.4-p23/bin/bash
export PATH=$PATH:/nix/store/xs38mci2dj4isxayifazwdma1dk9w9bl-coreutils-8.32/bin
set -euo pipefail
mkdir -p "$(dirname "/ipc/node.socket")"
echo "Starting: /nix/store/aginrkjcnlzbxsvla5bd8lnx73a475yi-cardano-node-exe-cardano-node-1.33.0/bin/cardano-node run"
   echo "--config /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json"
   echo "--database-path /data/db"
   echo "--topology /nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml"
   echo "--host-addr 0.0.0.0"
   echo "--port 3001"
   echo "--socket-path /ipc/node.socket"
   echo ""
   echo ""
   echo ""
   echo ""
   echo ""
   echo "+RTS"
   echo "-N2"
   echo "-I0"
   echo "-A16m"
   echo "-qg"
   echo "-qb"
   echo "--disable-delayed-os-memory-return"
   echo "-RTS"
echo "..or, once again, in a single line:"
echo "/nix/store/aginrkjcnlzbxsvla5bd8lnx73a475yi-cardano-node-exe-cardano-node-1.33.0/bin/cardano-node run --config /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json --database-path /data/db --topology /nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml --host-addr 0.0.0.0 --port 3001 --socket-path /ipc/node.socket      +RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS"

exec /nix/store/aginrkjcnlzbxsvla5bd8lnx73a475yi-cardano-node-exe-cardano-node-1.33.0/bin/cardano-node run --config /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json --database-path /data/db --topology /nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml --host-addr 0.0.0.0 --port 3001 --socket-path /ipc/node.socket      +RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS $@
bash-4.4# 

bash-4.4# cat /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json
{"AlonzoGenesisFile":"/nix/store/8qnphq6yvcjspiy3z0aijfd6cv64l3hl-alonzo-genesis.json","AlonzoGenesisHash":"7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874","ApplicationName":"cardano-sl","ApplicationVersion":0,"ByronGenesisFile":"/nix/store/kax0css4lx3ywihvsgrqjym0jpi20f99-byron-genesis.json","ByronGenesisHash":"96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471","LastKnownBlockVersion-Alt":0,"LastKnownBlockVersion-Major":3,"LastKnownBlockVersion-Minor":0,"MaxKnownMajorProtocolVersion":2,"Protocol":"Cardano","RequiresNetworkMagic":"RequiresMagic","ShelleyGenesisFile":"/nix/store/2xhy92909anynqsvx1b1x153cxwnfmzx-shelley-genesis.json","ShelleyGenesisHash":"849a1764f152e1b09c89c0dfdbcbdd38d711d1fec2db5dfa0f87cf2737a0eaf4","TraceAcceptPolicy":true,"TraceBlockFetchClient":false,"TraceBlockFetchDecisions":false,"TraceBlockFetchProtocol":false,"TraceBlockFetchProtocolSerialised":false,"TraceBlockFetchServer":false,"TraceChainDb":true,"TraceChainSyncBlockServer":false,"TraceChainSyncClient":false,"TraceChainSyncHeaderServer":false,"TraceChainSyncProtocol":false,"TraceConnectionManager":true,"TraceDNSResolver":true,"TraceDNSSubscription":true,"TraceDiffusionInitialization":true,"TraceErrorPolicy":true,"TraceForge":true,"TraceHandshake":false,"TraceInboundGovernor":true,"TraceIpSubscription":true,"TraceLedgerPeers":true,"TraceLocalChainSyncProtocol":false,"TraceLocalErrorPolicy":true,"TraceLocalHandshake":false,"TraceLocalRootPeers":true,"TraceLocalTxSubmissionProtocol":false,"TraceLocalTxSubmissionServer":false,"TraceMempool":true,"TraceMux":false,"TracePeerSelection":true,"TracePeerSelectionActions":true,"TracePublicRootPeers":true,"TraceServer":true,"TraceTxInbound":false,"TraceTxOutbound":false,"TraceTxSubmissionProtocol":false,"TracingVerbosity":"NormalVerbosity","TurnOnLogMetrics":true,"TurnOnLogging":true,"defaultBackends":["KatipBK"],"defaultScribes":[["StdoutSK","stdout"]],"hasEKG":12788,"hasPrometheus":["127.0.0.1",12798],"minSeverity":"Info","options":{"mapBackends":{"cardano.node.metrics":["EKGViewBK"],"cardano.node.resources":["EKGViewBK"]},"mapSubtrace":{"cardano.node.metrics":{"subtrace":"Neutral"}}},"rotation":{"rpKeepFilesNum":10,"rpLogLimitBytes":5000000,"rpMaxAgeHours":24},"setupBackends":["KatipBK"],"setupScribes":[{"scFormat":"ScText","scKind":"StdoutSK","scName":"stdout","scRotation":null}]}bash-4.4# 
bash-4.4# 
bash-4.4# cat /nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml
{"Producers":[{"addr":"relays-new.cardano-testnet.iohkdev.io","port":3001,"valency":1}]}bash-4.4# 


TestNet

        ┃       │ COMMAND      PID     USER   FD   TYPE DEVICE SIZE/OFF NODE NAME
        ┃       │ cardano-n 113982 lawrence   38u  IPv4 394820      0t0  TCP 127.0.0.1:35741 (LISTEN)
        ┃       │ COMMAND      PID     USER   FD   TYPE DEVICE SIZE/OFF NODE NAME
        ┃       │ cardano-n 113973 lawrence   38u  IPv4 395483      0t0  TCP 127.0.0.1:52577 (LISTEN)
        ┃       │ COMMAND      PID     USER   FD   TYPE DEVICE SIZE/OFF NODE NAME
        ┃       │ cardano-n 113964 lawrence   38u  IPv4 395487      0t0  TCP 127.0.0.1:44815 (LISTEN)


    37 ┃ mkConf tempAbsPath maybeMagic = do
    38 ┃   testnetMagic <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
       ┃   │ 1723
    39 ┃   tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
       ┃   │ "/tmp/chairman"
    40 ┃   tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
       ┃   │ "test-f48e1b65db1a86ae"
    41 ┃   base <- H.noteShowM H.getProjectBase
       ┃   │ ".."
    42 ┃   socketDir <- H.noteShow $ tempRelPath </> "socket"
       ┃   │ "test-f48e1b65db1a86ae/socket"
    43 ┃   logDir <- H.noteTempFile tempAbsPath "logs"
       ┃   │ /tmp/chairman/test-f48e1b65db1a86ae/logs



 │ CWD: Just "/tmp/chairman"
        ┃     │ Command line: /home/lawrence/src/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-node-1.33.0/x/cardano-node/build/cardano-node/cardano-node run --config /tmp/chairman/test-f48e1b65db1a86ae/configuration.yaml --topology /tmp/chairman/test-f48e1b65db1a86ae/node-praos1/topology.json --database-path /tmp/chairman/test-f48e1b65db1a86ae/node-praos1/db --shelley-kes-key /tmp/chairman/test-f48e1b65db1a86ae/node-praos1/kes.skey --shelley-vrf-key /tmp/chairman/test-f48e1b65db1a86ae/node-praos1/vrf.skey --shelley-operational-certificate /tmp/chairman/test-f48e1b65db1a86ae/node-praos1/node.cert --host-addr 127.0.0.1 --port 44815 --socket-path test-f48e1b65db1a86ae/socket/node-praos1
        ┃     │ CWD: Just "/tmp/chairman"
        ┃     │ Command line: /home/lawrence/src/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-node-1.33.0/x/cardano-node/build/cardano-node/cardano-node run --config /tmp/chairman/test-f48e1b65db1a86ae/configuration.yaml --topology /tmp/chairman/test-f48e1b65db1a86ae/node-praos2/topology.json --database-path /tmp/chairman/test-f48e1b65db1a86ae/node-praos2/db --shelley-kes-key /tmp/chairman/test-f48e1b65db1a86ae/node-praos2/kes.skey --shelley-vrf-key /tmp/chairman/test-f48e1b65db1a86ae/node-praos2/vrf.skey --shelley-operational-certificate /tmp/chairman/test-f48e1b65db1a86ae/node-praos2/node.cert --host-addr 127.0.0.1 --port 52577 --socket-path test-f48e1b65db1a86ae/socket/node-praos2
        ┃     │ CWD: Just "/tmp/chairman"
        ┃     │ Command line: /home/lawrence/src/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-node-1.33.0/x/cardano-node/build/cardano-node/cardano-node run --config /tmp/chairman/test-f48e1b65db1a86ae/configuration.yaml --topology /tmp/chairman/test-f48e1b65db1a86ae/node-pool1/topology.json --database-path /tmp/chairman/test-f48e1b65db1a86ae/node-pool1/db --shelley-kes-key /tmp/chairman/test-f48e1b65db1a86ae/node-pool1/kes.skey --shelley-vrf-key /tmp/chairman/test-f48e1b65db1a86ae/node-pool1/vrf.skey --shelley-operational-certificate /tmp/chairman/test-f48e1b65db1a86ae/node-pool1/node.cert --host-addr 127.0.0.1 --port 35741 --socket-path test-f48e1b65db1a86ae/socket/node-pool1




    397 ┃     -- So we'll need to sign this with a bunch of keys:
    398 ┃     -- 1. the initial utxo spending key, for the funds
    399 ┃     -- 2. the user n stake address key, due to the delegatation cert
    400 ┃     -- 3. the pool n owner key, due to the pool registration cert
    401 ┃     -- 3. the pool n operator key, due to the pool registration cert
    402 ┃ 
    403 ┃     void $ H.execCli
    404 ┃       [ "transaction", "sign"
    405 ┃       , "--signing-key-file", tempAbsPath </> "utxo-keys/utxo" <> n <> ".skey"
    406 ┃       , "--signing-key-file", tempAbsPath </> "addresses/user" <> n <> "-stake.skey"
    407 ┃       , "--signing-key-file", tempAbsPath </> "node-pool" <> n <> "/owner.skey"
    408 ┃       , "--signing-key-file", tempAbsPath </> "node-pool" <> n <> "/operator.skey"
    409 ┃       , "--testnet-magic", show @Int testnetMagic
    410 ┃       , "--tx-body-file", tempAbsPath </> "tx" <> n <> ".txbody"
    411 ┃       , "--out-file", tempAbsPath </> "tx" <> n <> ".tx"




 -- Now we'll construct one whopper of a transaction that does everything
    367 ┃   -- just to show off that we can, and to make the script shorter
    368 ┃ 
    369 ┃   forM_ userPoolN $ \n -> do
    370 ┃     -- We'll transfer all the funds to the user n, which delegates to pool n
    371 ┃     -- We'll register certs to:
    372 ┃     --  1. register the pool-owner n stake address
    373 ┃     --  2. register the stake pool n
    374 ┃     --  3. register the usern stake address
    375 ┃     --  4. delegate from the usern stake address to the stake pool
    376 ┃     genesisTxinResult <- H.noteShowM $ S.strip <$> H.execCli
    377 ┃       [ "genesis", "initial-txin"
    378 ┃       , "--testnet-magic", show @Int testnetMagic
    379 ┃       , "--verification-key-file", tempAbsPath </> "utxo-keys/utxo" <> n <> ".vkey"
    380 ┃       ]
        ┃       │ Command: /home/lawrence/src/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-cli-1.33.0/x/cardano-cli/build/cardano-cli/cardano-cli genesis initial-txin --testnet-magic 1723 --verification-key-file /tmp/chairman/test-f48e1b65db1a86ae/utxo-keys/utxo1.vkey
        ┃       │ "5ebe113780f66c358d3da6348453ef9f6b019c3484e63f03b522e271b61c6f15#0"
    381 ┃ 
    382 ┃     userNAddr <- H.readFile $ tempAbsPath </> "addresses/user" <> n <> ".addr"
        ┃     │ Reading file: /tmp/chairman/test-f48e1b65db1a86ae/addresses/user1.addr
    383 ┃ 
    384 ┃     void $ H.execCli
    385 ┃       [ "transaction", "build-raw"
    386 ┃       , "--invalid-hereafter", "1000"
    387 ┃       , "--fee", "0"
    388 ┃       , "--tx-in", genesisTxinResult
    389 ┃       , "--tx-out", userNAddr <> "+" <> show @Integer (maxLovelaceSupply testnetOptions)
    390 ┃       , "--certificate-file", tempAbsPath </> "addresses/pool-owner" <> n <> "-stake.reg.cert"
    391 ┃       , "--certificate-file", tempAbsPath </> "node-pool" <> n <> "/registration.cert"
    392 ┃       , "--certificate-file", tempAbsPath </> "addresses/user" <> n <> "-stake.reg.cert"
    393 ┃       , "--certificate-file", tempAbsPath </> "addresses/user" <> n <> "-stake.deleg.cert"
    394 ┃       , "--out-file", tempAbsPath </> "tx" <> n <> ".txbody"
    395 ┃       ]



sudo docker run ---mount type=bind,source="/tmp/chairman/test-f48e1b65db1a86ae/node-praos1/db/",target=/data/db --mount type=bind,source="/tmp/chairman/test-f48e1b65db1a86ae/socket/",target=/ipc inputoutput/cardano-wallet


lawrence@lawrence-MacBookAir:~/src/cardano-wallet$ cardano-wallet serve --node-socket /tmp/chairman/test-01b1a237b02e1abc/socket/node-praos1 --database ./wallet-db --listen-address 127.0.0.1 --port 5555 --testnet ./dist-newstyle/src/plutus-5e27fa0483bd0166/plutus-pab/test-node/testnet/testnet-byron-genesis.json
[cardano-wallet.main:Info:4] [2022-01-28 21:39:53.18 UTC] Running as v2022-01-18 (git revision: a5085acbd2670c24251cf8d76a4e83c77a2679ba) on x86_64-linux
[cardano-wallet.main:Info:4] [2022-01-28 21:39:53.18 UTC] Command line: /home/lawrence/.local/bin/cardano-wallet serve --node-socket /tmp/chairman/test-01b1a237b02e1abc/socket/node-praos1 --database ./wallet-db --listen-address 127.0.0.1 --port 5555 --testnet ./dist-newstyle/src/plutus-5e27fa0483bd0166/plutus-pab/test-node/testnet/testnet-byron-genesis.json
[cardano-wallet.main:Debug:4] [2022-01-28 21:39:53.18 UTC] ServeArgs {_hostPreference = Host "127.0.0.1", _listen = ListenOnPort 5555, _tlsConfig = Nothing, _nodeSocket = CardanoNodeConn "/tmp/chairman/test-01b1a237b02e1abc/socket/node-praos1", _networkConfiguration = TestnetConfig "./dist-newstyle/src/plutus-5e27fa0483bd0166/plutus-pab/test-node/testnet/testnet-byron-genesis.json", _database = Just "./wallet-db", _syncTolerance = SyncTolerance 300s, _enableShutdownHandler = False, _poolMetadataSourceOpt = Nothing, _tokenMetadataSourceOpt = Nothing, _logging = LoggingOptions {loggingMinSeverity = Debug, loggingTracers = Tracers {applicationTracer = Const (Just Info), apiServerTracer = Const (Just Info), tokenMetadataTracer = Const (Just Info), walletEngineTracer = Const (Just Info), walletDbTracer = Const (Just Info), poolsEngineTracer = Const (Just Info), poolsDbTracer = Const (Just Info), ntpClientTracer = Const (Just Info), networkTracer = Const (Just Info)}, loggingTracersDoc = Nothing}}
[cardano-wallet.main:Info:4] [2022-01-28 21:39:53.21 UTC] Wallet databases: Using directory: ./wallet-db
[cardano-wallet.application:Info:4] [2022-01-28 21:39:53.21 UTC] Wallet backend server starting. Using /tmp/chairman/test-01b1a237b02e1abc/socket/node-praos1.
[cardano-wallet.application:Info:4] [2022-01-28 21:39:53.22 UTC] Node is Haskell Node on testnet (1097911063).
[cardano-wallet.token-metadata:Notice:4] [2022-01-28 21:39:53.22 UTC] No token metadata server is configured.
[cardano-wallet.token-metadata:Notice:4] [2022-01-28 21:39:53.22 UTC] No token metadata server is configured.
[cardano-wallet.token-metadata:Notice:4] [2022-01-28 21:39:53.22 UTC] No token metadata server is configured.
[cardano-wallet.token-metadata:Notice:4] [2022-01-28 21:39:53.22 UTC] No token metadata server is configured.
[cardano-wallet.pools-db:Info:4] [2022-01-28 21:39:53.22 UTC] Starting connection pool for ./wallet-db/stake-pools.sqlite
[cardano-wallet.pools-db:Info:4] [2022-01-28 21:39:53.25 UTC] Closing single database connection (./wallet-db/stake-pools.sqlite)
[cardano-wallet.pools-db:Info:4] [2022-01-28 21:39:53.26 UTC] Stopping database connection pool ./wallet-db/stake-pools.sqlite
[cardano-wallet.main:Debug:4] [2022-01-28 21:39:53.32 UTC] Logging shutdown.
cardano-wallet: ExceptionInLinkedThread (ThreadId 18) (HandshakeError (VersionMismatch [NodeToClientV_1,NodeToClientV_2,NodeToClientV_3,NodeToClientV_4,NodeToClientV_5] []))


-- | The same as 'runGuardedStep' but we can supply additional constraints and lookups for transaction.
runGuardedStepWith ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)    -- ^ Additional lookups
    -> TxConstraints input state                   -- ^ Additional constraints
    -> StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStepWith userLookups userConstraints smc input guard = mapError (review _SMContractError) $ mkStep smc input >>= \case
    Right StateMachineTransition{smtConstraints,smtOldState=State{stateData=os}, smtNewState=State{stateData=ns}, smtLookups} -> do
        pk <- ownPaymentPubKeyHash
        let lookups = smtLookups { Constraints.slOwnPaymentPubKeyHash = Just pk }
        utx <- either (throwing _ConstraintResolutionError)
                      pure
                      (Constraints.mkTx (lookups <> userLookups) (smtConstraints <> userConstraints))
        let adjustedUtx = Constraints.adjustUnbalancedTx utx
        unless (utx == adjustedUtx) $
          logWarn @Text $ "Plutus.Contract.StateMachine.runStep: "
                       <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
        case guard adjustedUtx os ns of
            Nothing -> do
                submitTxConfirmed adjustedUtx
                pure $ Right $ TransitionSuccess ns
            Just a  -> pure $ Left a
    Left e -> pure $ Right $ TransitionFailure e



    https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/src/Plutus.Contract.Wallet.html


Failed to build plutus-core-0.1.0.0.
Build log (
/home/lawrence/.cabal/logs/ghc-8.10.4.20210212/plutus-core-0.1.0.0-44ad8ab5dc3de4f7a9a9bd42a1e2cb1a43cab71aa7db76fbb490c0cd6dda983c.log
):
Configuring library for plutus-core-0.1.0.0..
Preprocessing library for plutus-core-0.1.0.0..
/home/lawrence/.cabal/store/ghc-8.10.4.20210212/alex-3.2.6-e-alex-e2773d6a02960d93b03dd799dca5e8dc38d008c10e51820ebaf57a5b31ae7c47/bin/alex: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
cabal: Failed to build plutus-core-0.1.0.0 (which is required by
test:plutus-use-cases-test from plutus-use-cases-0.1.0.0,
exe:plutus-use-cases-scripts from plutus-use-cases-0.1.0.0 and others). See
the build log above for details.


   [nix-shell:~/src/plutus-apps]$ rm -rf  /home/lawrence/.cabal/store/ghc-8.10.4.20210212/happy-1.20.0-e0c5b7ba/ directory and run cabal build again.

    [nix-shell:~/src/plutus-apps]$ rm -rf /home/lawrence/.cabal/store/ghc-8.10.4.20210212/alex-3.2.6-e-alex-e2773d6a02960d93b03dd799dca5e8dc38d008c10e51820ebaf57a5b31ae7c47

lawrence@lawrence-MacBookAir:~/src/cardano-lottery$ cat /etc/nix/nix.conf 
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
sandbox = false
experimental-features = nix-command
extra-experimental-features = flakes

###################################### JAN 29 #####################################


curl -s http://localhost:9080/api/contract/definitions | jq

curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq

/run/user/1000/


[pab:Info:1771] [2022-01-30 02:56:27.70 UTC] 468c3e68-2033-456d-b30f-b62b0ab4a7e1: "lotto has been intialized Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = ebe26d1fd6a2cc5318a5847b2f07787e803d48f48055e42f20d12fa1640d291a, txOutRefIdx = 0}, ttCurrencySymbol = f00ed6226186260df24f2f6ec136cc1d361077ea3480fe6742fca5a3})}"
Current block: 314. Current slot: 4493


[pab:Warning:1838] [2022-01-30 03:16:19.23 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/proxy/transactions\"), requestQueryString = fromList [], requestBody = Just ((),application/octet-stream), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Sun, 30 Jan 2022 03:16:18 GMT\"),(\"Server\",\"Warp/3.3.18\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The submitted transaction was rejected by the local node. Here's an error message that may help with debugging: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch (IsValid True) (FailedUnexpectedly [PlutusFailure \\\\\\\"\\\\\\\\nThe 3 arg plutus script (PlutusScript PlutusV1 ScriptHash \\\\\\\\\\\\\\\"66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e\\\\\\\\\\\\\\\") fails.\\\\\\\\nCekError An error has occurred:  User error:\\\\\\\\nThe budget was overspent. Final negative state: ({ cpu: -6916 | mem: 0 })\\\\\\\\nThe data is: Constr 0 [Constr 0 [B \\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",I 1596064091999,I 20000,B \\\\\\\\\\\\\\\"0\\\\\\\\\\\\\\\",List [Constr 0 [B \\\\\\\\\\\\\\\"\\\\\\\\\\\\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\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",B \\\\\\\\\\\\\\\"0\\\\\\\\\\\\\\\"]],B \\\\\\\\\\\\\\\"J\\\\\\\\\\\\\\\\145\\\\\\\\\\\\\\\\186~(\\\\\\\\\\\\\\\\180@\\\\\\\\\\\\\\\\236\\\\\\\\\\\\\\\\144\\\\\\\\\\\\\\\\131\\\\\\\\\\\\\\\\220\\\\\\\\\\\\\\\\&1FFr\\\\\\\\\\\\\\\\170\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\228\\\\\\\\\\\\\\\\188\\\\\\\\\\\\\\\\166\\\\\\\\\\\\\\\\253V}\\\\\\\\\\\\\\\\175\\\\\\\\\\\\\\\\vU\\\\\\\\\\\\\\\\252\\\\\\\\\\\\\\\",I 10000000,I 0,I 0,I 0,Map [(B \\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",I 0)]]]\\\\\\\\nThe redeemer is: Constr 1 [Constr 0 [B \\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",B \\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\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\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",I 1596064091999,I 20000,I 10000000],B \\\\\\\\\\\\\\\"0-\\\\\\\\\\\\\\\"]\\\\\\\\nThe context is:\\\\\\\\nPurpose: Spending (TxOutRef {txOutRefId = 0ec9e4484040b16fb726906e43395a2451b94c6bc543a715218cc684242a9b9e, txOutRefIdx = 0})\\\\\\\\nTxInfo:\\\\\\\\n  TxId: f55f141f21fcb1999d501509ce006665ec5296eb197628580b6a571d8a6ef0c8\\\\\\\\n  Inputs: [ 0ec9e4484040b16fb726906e43395a2451b94c6bc543a715218cc684242a9b9e!0 -> - Value (Map [(,Map [(\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\",10000000)]),(f00ed6226186260df24f2f6ec136cc1d361077ea3480fe6742fca5a3,Map [(0x66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e,1)])]) addressed to\\\\\\\\n                                                                                    ScriptCredential: 66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e (no staking credential)\\\\\\\\n          , 0ec9e4484040b16fb726906e43395a2451b94c6bc543a715218cc684242a9b9e!1 -> - Value (Map [(,Map [(\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\",989512866)])]) addressed to\\\\\\\\n                                                                                    PubKeyCredential: 03e809223e59ba762d046ef508c53725a1ebe2e926f22e2c8976d4cf (StakingHash PubKeyCredential: 0570132368246ec2410c5a1a15be3c9e82997a473b98440bea1dba0b) ]\\\\\\\\n  Outputs: [ - Value (Map [(,Map [(\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\",20000000)]),(f00ed6226186260df24f2f6ec136cc1d361077ea3480fe6742fca5a3,Map [(0x66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e,1)])]) addressed to\\\\\\\\n               ScriptCredential: 66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e (no staking credential)\\\\\\\\n           , - Value (Map [(,Map [(\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\",978016659)])]) addressed to\\\\\\\\n               PubKeyCredential: 381aa5ad89102a088674c8e246a6fc9909dd2818f3725df9aef51f89 (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 PubKeyCredential: 0570132368246ec2410c5a1a15be3c9e82997a473b98440bea1dba0b) ]\\\\\\\\n  Fee: Value (Map [(,Map [(\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\",1496207)])])\\\\\\\\n  Value minted: Value (Map [])\\\\\\\\n  DCerts: []\\\\\\\\n  Wdrl: []\\\\\\\\n  Valid range: (-\\\\\\\\8734 , +\\\\\\\\8734)\\\\\\\\n  Signatories: [8e2f209d8cac82ee65fe378d1cddb0b0f72640666a1c9cb85cd2a28f]\\\\\\\\n  Datums: [ ( 98bb28ce73d89cb97550c6e2c3fd84c5a0dfd81f52682d44661d1066ea9a36cf\\\\\\\\n          , <<\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",\\\\\\\\n          1596064091999,\\\\\\\\n          20000,\\\\\\\\n          \\\\\\\\\\\\\\\"0\\\\\\\\\\\\\\\",\\\\\\\\n          [<\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",\\\\\\\\n          \\\\\\\\\\\\\\\"0\\\\\\\\\\\\\\\">],\\\\\\\\n          \\\\\\\\\\\\\\\"J\\\\\\\\\\\\\\\\145\\\\\\\\\\\\\\\\186~(\\\\\\\\\\\\\\\\180@\\\\\\\\\\\\\\\\236\\\\\\\\\\\\\\\\144\\\\\\\\\\\\\\\\131\\\\\\\\\\\\\\\\220\\\\\\\\\\\\\\\\&1FFr\\\\\\\\\\\\\\\\170\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\228\\\\\\\\\\\\\\\\188\\\\\\\\\\\\\\\\166\\\\\\\\\\\\\\\\253V}\\\\\\\\\\\\\\\\175\\\\\\\\\\\\\\\\vU\\\\\\\\\\\\\\\\252\\\\\\\\\\\\\\\",\\\\\\\\n          10000000,\\\\\\\\n          0,\\\\\\\\n          0,\\\\\\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\\n          0,\\\\\\\\n          {\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\": 0}>> )\\\\\\\\n          , ( e0c8040d412fcf8b6f315d49808405019997a29b0d22abf9fbfc54ac4f4c4271\\\\\\\\n          , <<\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",\\\\\\\\n          1596064091999,\\\\\\\\n          20000,\\\\\\\\n          \\\\\\\\\\\\\\\"0-\\\\\\\\\\\\\\\",\\\\\\\\n          [<\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\",\\\\\\\\n          \\\\\\\\\\\\\\\"0-\\\\\\\\\\\\\\\">],\\\\\\\\n          \\\\\\\\\\\\\\\"J\\\\\\\\\\\\\\\\145\\\\\\\\\\\\\\\\186~(\\\\\\\\\\\\\\\\180@\\\\\\\\\\\\\\\\236\\\\\\\\\\\\\\\\144\\\\\\\\\\\\\\\\131\\\\\\\\\\\\\\\\220\\\\\\\\\\\\\\\\&1FFr\\\\\\\\\\\\\\\\170\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\228\\\\\\\\\\\\\\\\188\\\\\\\\\\\\\\\\166\\\\\\\\\\\\\\\\253V}\\\\\\\\\\\\\\\\175\\\\\\\\\\\\\\\\vU\\\\\\\\\\\\\\\\252\\\\\\\\\\\\\\\",\\\\\\\\n          20000000,\\\\\\\\n          1,\\\\\\\\n          0,\\\\\\\\n          0,\\\\\\\\n          {\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\142/ \\\\\\\\\\\\\\\\157\\\\\\\\\\\\\\\\140\\\\\\\\\\\\\\\\172\\\\\\\\\\\\\\\\130\\\\\\\\\\\\\\\\238e\\\\\\\\\\\\\\\\254\\\\\\\\\\\\\\\\&7\\\\\\\\\\\\\\\\141\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\221\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\176\\\\\\\\\\\\\\\\247&@fj\\\\\\\\\\\\\\\\FS\\\\\\\\\\\\\\\\156\\\\\\\\\\\\\\\\184\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\210\\\\\\\\\\\\\\\\162\\\\\\\\\\\\\\\\143\\\\\\\\\\\\\\\": 0}>> ) ]\\\\\\\\n\\\\\\\" \\\\\\\"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\"])))))])) AlonzoEraInCardanoMode"}
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


cardano-wallet.wallet-engine:Info:1885] [2022-01-30 03:16:18.70 UTC] {"string":"b6ac58e4: Selection report (summarized):\nSelectionReportSummarized:\n  computedFee: 1.503700\n  adaBalanceOfSelectedInputs: 999.512866\n  adaBalanceOfExtraCoinSource: 0.000000\n  adaBalanceOfExtraCoinSink: 0.000000\n  adaBalanceOfRequestedOutputs: 20.000000\n  adaBalanceOfGeneratedChangeOutputs: 978.009166\n  numberOfSelectedInputs: 2\n  numberOfSelectedCollateralInputs: 1\n  numberOfRequestedOutputs: 1\n  numberOfGeneratedChangeOutputs: 1\n  numberOfUniqueNonAdaAssetsInSelectedInputs: 1\n  numberOfUniqueNonAdaAssetsInRequestedOutputs: 1\n  numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs: 0\n"}
[cardano-wallet.api-server:Info:1885] [2022-01-30 03:16:18.74 UTC] {"string":"[RequestId 14] POST /v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-balance 202 Accepted in 0.046750028s"}
[cardano-wallet.api-server:Info:1885] [2022-01-30 03:16:18.76 UTC] {"string":"[RequestId 15] [POST] /v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-sign"}
[cardano-wallet.api-server:Info:1885] [2022-01-30 03:16:19.20 UTC] {"string":"[RequestId 15] POST /v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-sign 202 Accepted in 0.440057186s"}
[cardano-wallet.api-server:Info:1885] [2022-01-30 03:16:19.21 UTC] {"string":"[RequestId 16] [POST] /v2/proxy/transactions"}
[cardano-wallet.wallet-engine:Info:1885] [2022-01-30 03:16:19.21 UTC] {"string":"Submitting external transaction f55f141f to local node..."}
[cardano-wallet.wallet-engine:Error:1885] [2022-01-30 03:16:19.23 UTC] {"string":"Transaction f55f141f failed: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch (IsValid True) (FailedUnexpectedly [PlutusFailure \"\\nThe 3 arg plutus script (PlutusScript PlutusV1 ScriptHash \\\"66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e\\\") fails.\\nCekError An error has occurred:  User error:\\nThe budget was overspent. Final negative state: ({ cpu: -6916 | mem: 0 })\\nThe data is: Constr 0 [Constr 0 [B \\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",I 1596064091999,I 20000,B \\\"0\\\",List [Constr 0 [B \\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",B \\\"0\\\"]],B \\\"J\\\\145\\\\186~(\\\\180@\\\\236\\\\144\\\\131\\\\220\\\\&1FFr\\\\170\\\\130\\\\FS\\\\228\\\\188\\\\166\\\\253V}\\\\175\\\\vU\\\\252\\\",I 10000000,I 0,I 0,I 0,Map [(B \\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",I 0)]]]\\nThe redeemer is: Constr 1 [Constr 0 [B \\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",B \\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",I 1596064091999,I 20000,I 10000000],B \\\"0-\\\"]\\nThe context is:\\nPurpose: Spending (TxOutRef {txOutRefId = 0ec9e4484040b16fb726906e43395a2451b94c6bc543a715218cc684242a9b9e, txOutRefIdx = 0})\\nTxInfo:\\n  TxId: f55f141f21fcb1999d501509ce006665ec5296eb197628580b6a571d8a6ef0c8\\n  Inputs: [ 0ec9e4484040b16fb726906e43395a2451b94c6bc543a715218cc684242a9b9e!0 -> - Value (Map [(,Map [(\\\"\\\",10000000)]),(f00ed6226186260df24f2f6ec136cc1d361077ea3480fe6742fca5a3,Map [(0x66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e,1)])]) addressed to\\n                                                                                    ScriptCredential: 66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e (no staking credential)\\n          , 0ec9e4484040b16fb726906e43395a2451b94c6bc543a715218cc684242a9b9e!1 -> - Value (Map [(,Map [(\\\"\\\",989512866)])]) addressed to\\n                                                                                    PubKeyCredential: 03e809223e59ba762d046ef508c53725a1ebe2e926f22e2c8976d4cf (StakingHash PubKeyCredential: 0570132368246ec2410c5a1a15be3c9e82997a473b98440bea1dba0b) ]\\n  Outputs: [ - Value (Map [(,Map [(\\\"\\\",20000000)]),(f00ed6226186260df24f2f6ec136cc1d361077ea3480fe6742fca5a3,Map [(0x66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e,1)])]) addressed to\\n               ScriptCredential: 66299662a557e5777934177fcb1bd3b4549b1cfc0ecc17e92738b26e (no staking credential)\\n           , - Value (Map [(,Map [(\\\"\\\",978016659)])]) addressed to\\n               PubKeyCredential: 381aa5ad89102a088674c8e246a6fc9909dd2818f3725df9aef51f89 (StakingHash PubKeyCredential: 0570132368246ec2410c5a1a15be3c9e82997a473b98440bea1dba0b) ]\\n  Fee: Value (Map [(,Map [(\\\"\\\",1496207)])])\\n  Value minted: Value (Map [])\\n  DCerts: []\\n  Wdrl: []\\n  Valid range: (-\\8734 , +\\8734)\\n  Signatories: [8e2f209d8cac82ee65fe378d1cddb0b0f72640666a1c9cb85cd2a28f]\\n  Datums: [ ( 98bb28ce73d89cb97550c6e2c3fd84c5a0dfd81f52682d44661d1066ea9a36cf\\n          , <<\\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",\\n          1596064091999,\\n          20000,\\n          \\\"0\\\",\\n          [<\\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",\\n          \\\"0\\\">],\\n          \\\"J\\\\145\\\\186~(\\\\180@\\\\236\\\\144\\\\131\\\\220\\\\&1FFr\\\\170\\\\130\\\\FS\\\\228\\\\188\\\\166\\\\253V}\\\\175\\\\vU\\\\252\\\",\\n          10000000,\\n          0,\\n          0,\\n          0,\\n          {\\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\": 0}>> )\\n          , ( e0c8040d412fcf8b6f315d49808405019997a29b0d22abf9fbfc54ac4f4c4271\\n          , <<\\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",\\n          1596064091999,\\n          20000,\\n          \\\"0-\\\",\\n          [<\\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\",\\n          \\\"0-\\\">],\\n          \\\"J\\\\145\\\\186~(\\\\180@\\\\236\\\\144\\\\131\\\\220\\\\&1FFr\\\\170\\\\130\\\\FS\\\\228\\\\188\\\\166\\\\253V}\\\\175\\\\vU\\\\252\\\",\\n          20000000,\\n          1,\\n          0,\\n          0,\\n          {\\\"\\\\142/ \\\\157\\\\140\\\\172\\\\130\\\\238e\\\\254\\\\&7\\\\141\\\\FS\\\\221\\\\176\\\\176\\\\247&@fj\\\\FS\\\\156\\\\184\\\\\\\\\\\\210\\\\162\\\\143\\\": 0}>> ) ]\\n\" \"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\"])))))])) AlonzoEraInCardanoMode"}
[cardano-wallet.api-server:Error:1885] [2022-01-30 03:16:19.23 UTC] {"string":"[RequestId 16] POST /v2/proxy/transactions 500 Internal Server Error in 0.026267505s"}






cabal exec -- cardano-lottery-pab-run --config cardano-lottery-pab.yaml migrate

cabal exec -- cardano-lottery-pab-run --config cardano-lottery-pab.yaml webserver --passphrase cardano-wallet


curl -H "Content-Type: application/json" -v -X POST -d \
  "{\"caID\":{\"tag\":\"IntegrationTest\"},\"caWallet\":{\"getWalletId\":\"$WALLET_ID\"}}" \
  localhost:9080/api/contract/activate


  [cardano-wallet.wallet-engine:Info:12527] [2022-01-30 19:10:13.41 UTC] Submitting external transaction 56f229e2 to local node...
[cardano-wallet.wallet-engine:Info:12527] [2022-01-30 19:10:13.45 UTC] Transaction 56f229e2 accepted by local node
tx:
  56f229e2
  []
  inputs 1st ca04be47
  outputs address: 7078512d...645f9ce6
          coin: 10.000000
          tokens: []
  outputs address: 0026ca8b...f2be7e9d
          coin: 989.829967
          tokens: []
  []
  metadata:
  scriptValidity: valid

[cardano-wallet.api-server:Info:12527] [2022-01-30 19:10:13.45 UTC] [RequestId 18] POST /v2/proxy/transactions 202 Accepted in 0.048404927s

Received Time
> 17 minutes ago (2022-01-30 19:10:39 UTC)
Included In
Epoch 184, Block 3280057
Confirmations
43
Transaction ID
56f229e2ab8dab7e0e5e9d1615acc236dfc7feec1e1fe802c2029cf00554136b
From addresses
addr_test1qz5gks...qwdu4706wsya900t
1000 ADA
To addresses
addr_test1wpu9zt...kydv30eeesm9aw7a
10 ADA
addr_test1qqnv4z...qwdu4706ws0yarxq
989.829967 ADA
Total Output
999.829967 ADA
Transaction Fee
0.170033 ADA


cardano-wallet.network:Info:12] [2022-01-31 01:35:07.65 UTC] Protocol parameters for tip are:
 Decentralization level: 100.00%
 Transaction parameters: [Fee policy: 155381.0 + 44.0x, Tx max size: 16384, max exec units: max steps: 10000000000, max memory: 11250000]
 Desired number of pools: 500
 Minimum UTxO value: 0.034482 per word
 Eras:
   - byron from 0
   - shelley from 74
   - allegra from 102
   - mary from 112
   - alonzo from 154
 Execution unit prices: 721 % 10000000 per step, 577 % 10000 per memory unit

Slotting parameters for tip are:
 Slot length:        1s
 Epoch length:       432000
 Active slot coeff:  5.0e-2
 Security parameter: 2160 block



 ####################################### Jan 31 #################################

simpleproxy -L localhost:8090 -R localhost:46493


 export SHELLEY_TEST_DATA=~/src/plutus-apps/plutus-pab/local-cluster/cluster-data/cardano-node-shelley

$ cabal build cardano-lottery-pab-run
$ cabal exec cardano-lottery-pab-run


cardano-wallet.network:Info:1376] [2022-02-01 01:51:20.78 UTC] {"string":"Protocol parameters for tip are:\n Decentralization level: 75.00%\n Transaction parameters: [Fee policy: 100000.0 + 100.0x, Tx max size: 16384, max exec units: max steps: 100000000000, max memory: 100000000]\n Desired number of pools: 3\n Minimum UTxO value: 0.034482 per word\n Eras:\n   - byron from -0\n   - shelley from -0\n   - allegra from -0\n   - mary from -0\n   - alonzo from -0\n Execution unit prices: 721 % 10000000 per step, 577 % 10000 per memory unit\n\nSlotting parameters for tip are:\n Slot length:        0.2s\n Epoch length:       100\n Active slot coeff:  0.5\n Security parameter: 5 block\n\n"}
[c

InitLotto - init endpoint

[cardano-wallet.wallet-engine:Info:1883] [2022-02-01 02:37:04.86 UTC] {"string":"Submitting external transaction 43b65435 to local node..."}
[cardano-wallet.wallet-engine:Info:1883] [2022-02-01 02:37:04.87 UTC] {"string":"Transaction 43b65435 accepted by local node\ntx:\n  43b65435\n  collateral 1st 410d1022\n  inputs 1st 410d1022\n  outputs address: 71920283...f3293571\n          coin: 10.000000\n          tokens:\n            - policy: 88eca2a9a3923342231def085b50c8f9467e2842c3cdb14207a1fc4d\n              tokens:\n                - token: 920283987ab7e8039e6d2cd3b6a5c884b576fd45f45b8fe4f3293571\n                  quantity: 1\n  outputs address: 012cbd70...7e11d40c\n          coin: 989.512866\n          tokens: []\n  []\n  metadata:\n  scriptValidity: valid\n"}
[cardano-wallet.api-server:Info:1883] [2022-02-01 02:37:04.87 UTC] {"string":"[RequestId 19] POST /v2/proxy/transactions 202 Accepted in 0.014267784s"}
[cardano-wallet.wallet-engine:Info:1811] [2022-02-01 02:37:05.33 UTC] {"string":"09d26cc1: discovered 1 new transaction(s)"}



UseLottoContract - start endpoint

[cardano-wallet.wallet-engine:Error:1920] [2022-02-01 02:45:18.79 UTC] {"string":"Transaction c2ba5716 failed: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch (IsValid True) (FailedUnexpectedly [PlutusFailure \"\\nThe 3 arg plutus script (PlutusScript PlutusV1 ScriptHash \\\"920283987ab7e8039e6d2cd3b6a5c884b576fd45f45b8fe4f3293571\\\") fails.\\nCekError An error has occurred:  User error:\\nThe budget was overspent. Final negative state: ({ cpu: -6916 | mem: 0 }

plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Evaluation/Machine/Cek/Internal.hs


https://github.com/input-output-hk/cardano-node/issues/3470

