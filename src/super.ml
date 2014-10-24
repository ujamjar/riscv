(* supervisor register definitions, taken from spike *)
module Make(D : Types.D) = struct

  exception RISCV_invalid_csr

  type t =
    {
      mutable fflags   : D.t; 
      mutable frm      : D.t; 
      mutable fcsr     : D.t; 
      mutable stats    : D.t; 
      mutable sup0     : D.t; 
      mutable sup1     : D.t; 
      mutable epc      : D.t; 
      mutable badvaddr : D.t; 
      mutable ptbr     : D.t; 
      mutable asid     : D.t; 
      mutable count    : D.t; 
      mutable compare  : D.t; 
      mutable evec     : D.t; 
      mutable cause    : D.t; 
      mutable status   : D.t; 
      mutable hartid   : D.t; 
      mutable impl     : D.t; 
      mutable fatc     : D.t; 
      mutable send_ipi : D.t; 
      mutable clear_ipi: D.t; 
      mutable reset    : D.t; 
      mutable tohost   : D.t; 
      mutable fromhost : D.t; 
      mutable cycle    : D.t; 
      mutable time     : D.t; 
      mutable instret  : D.t; 
      mutable uarch0   : D.t; 
      mutable uarch1   : D.t; 
      mutable uarch2   : D.t; 
      mutable uarch3   : D.t; 
      mutable uarch4   : D.t; 
      mutable uarch5   : D.t; 
      mutable uarch6   : D.t; 
      mutable uarch7   : D.t; 
      mutable uarch8   : D.t; 
      mutable uarch9   : D.t; 
      mutable uarch10  : D.t; 
      mutable uarch11  : D.t; 
      mutable uarch12  : D.t; 
      mutable uarch13  : D.t; 
      mutable uarch14  : D.t; 
      mutable uarch15  : D.t; 
      mutable counth   : D.t; 
      mutable cycleh   : D.t; 
      mutable timeh    : D.t; 
      mutable instreth : D.t; 
    }

  type e = 
    | CSR_FFLAGS    
    | CSR_FRM       
    | CSR_FCSR      
    | CSR_STATS     
    | CSR_SUP0      
    | CSR_SUP1      
    | CSR_EPC       
    | CSR_BADVADDR  
    | CSR_PTBR      
    | CSR_ASID      
    | CSR_COUNT     
    | CSR_COMPARE   
    | CSR_EVEC      
    | CSR_CAUSE     
    | CSR_STATUS    
    | CSR_HARTID    
    | CSR_IMPL      
    | CSR_FATC      
    | CSR_SEND_IPI  
    | CSR_CLEAR_IPI 
    | CSR_RESET     
    | CSR_TOHOST    
    | CSR_FROMHOST  
    | CSR_CYCLE     
    | CSR_TIME      
    | CSR_INSTRET   
    | CSR_UARCH0    
    | CSR_UARCH1    
    | CSR_UARCH2    
    | CSR_UARCH3    
    | CSR_UARCH4    
    | CSR_UARCH5    
    | CSR_UARCH6    
    | CSR_UARCH7    
    | CSR_UARCH8    
    | CSR_UARCH9    
    | CSR_UARCH10   
    | CSR_UARCH11   
    | CSR_UARCH12   
    | CSR_UARCH13   
    | CSR_UARCH14   
    | CSR_UARCH15   
    | CSR_COUNTH    
    | CSR_CYCLEH    
    | CSR_TIMEH     
    | CSR_INSTRETH  

  let to_addr = function
    | CSR_FFLAGS    -> 0x1
    | CSR_FRM       -> 0x2
    | CSR_FCSR      -> 0x3
    | CSR_STATS     -> 0xc0
    | CSR_SUP0      -> 0x500
    | CSR_SUP1      -> 0x501
    | CSR_EPC       -> 0x502
    | CSR_BADVADDR  -> 0x503
    | CSR_PTBR      -> 0x504
    | CSR_ASID      -> 0x505
    | CSR_COUNT     -> 0x506
    | CSR_COMPARE   -> 0x507
    | CSR_EVEC      -> 0x508
    | CSR_CAUSE     -> 0x509
    | CSR_STATUS    -> 0x50a
    | CSR_HARTID    -> 0x50b
    | CSR_IMPL      -> 0x50c
    | CSR_FATC      -> 0x50d
    | CSR_SEND_IPI  -> 0x50e
    | CSR_CLEAR_IPI -> 0x50f
    | CSR_RESET     -> 0x51d
    | CSR_TOHOST    -> 0x51e
    | CSR_FROMHOST  -> 0x51f
    | CSR_CYCLE     -> 0xc00
    | CSR_TIME      -> 0xc01
    | CSR_INSTRET   -> 0xc02
    | CSR_UARCH0    -> 0xcc0
    | CSR_UARCH1    -> 0xcc1
    | CSR_UARCH2    -> 0xcc2
    | CSR_UARCH3    -> 0xcc3
    | CSR_UARCH4    -> 0xcc4
    | CSR_UARCH5    -> 0xcc5
    | CSR_UARCH6    -> 0xcc6
    | CSR_UARCH7    -> 0xcc7
    | CSR_UARCH8    -> 0xcc8
    | CSR_UARCH9    -> 0xcc9
    | CSR_UARCH10   -> 0xcca
    | CSR_UARCH11   -> 0xccb
    | CSR_UARCH12   -> 0xccc
    | CSR_UARCH13   -> 0xccd
    | CSR_UARCH14   -> 0xcce
    | CSR_UARCH15   -> 0xccf
    | CSR_COUNTH    -> 0x586
    | CSR_CYCLEH    -> 0xc80
    | CSR_TIMEH     -> 0xc81
    | CSR_INSTRETH  -> 0xc82

  let of_addr = function
    | 0x1    -> CSR_FFLAGS    
    | 0x2    -> CSR_FRM       
    | 0x3    -> CSR_FCSR      
    | 0xc0   -> CSR_STATS     
    | 0x500  -> CSR_SUP0      
    | 0x501  -> CSR_SUP1      
    | 0x502  -> CSR_EPC       
    | 0x503  -> CSR_BADVADDR  
    | 0x504  -> CSR_PTBR      
    | 0x505  -> CSR_ASID      
    | 0x506  -> CSR_COUNT     
    | 0x507  -> CSR_COMPARE   
    | 0x508  -> CSR_EVEC      
    | 0x509  -> CSR_CAUSE     
    | 0x50a  -> CSR_STATUS    
    | 0x50b  -> CSR_HARTID    
    | 0x50c  -> CSR_IMPL      
    | 0x50d  -> CSR_FATC      
    | 0x50e  -> CSR_SEND_IPI  
    | 0x50f  -> CSR_CLEAR_IPI 
    | 0x51d  -> CSR_RESET     
    | 0x51e  -> CSR_TOHOST    
    | 0x51f  -> CSR_FROMHOST  
    | 0xc00  -> CSR_CYCLE     
    | 0xc01  -> CSR_TIME      
    | 0xc02  -> CSR_INSTRET   
    | 0xcc0  -> CSR_UARCH0    
    | 0xcc1  -> CSR_UARCH1    
    | 0xcc2  -> CSR_UARCH2    
    | 0xcc3  -> CSR_UARCH3    
    | 0xcc4  -> CSR_UARCH4    
    | 0xcc5  -> CSR_UARCH5    
    | 0xcc6  -> CSR_UARCH6    
    | 0xcc7  -> CSR_UARCH7    
    | 0xcc8  -> CSR_UARCH8    
    | 0xcc9  -> CSR_UARCH9    
    | 0xcca  -> CSR_UARCH10   
    | 0xccb  -> CSR_UARCH11   
    | 0xccc  -> CSR_UARCH12   
    | 0xccd  -> CSR_UARCH13   
    | 0xcce  -> CSR_UARCH14   
    | 0xccf  -> CSR_UARCH15   
    | 0x586  -> CSR_COUNTH    
    | 0xc80  -> CSR_CYCLEH    
    | 0xc81  -> CSR_TIMEH     
    | 0xc82  -> CSR_INSTRETH  
    | _      -> raise RISCV_invalid_csr

  let set super r v = 
    match r with
    | CSR_FFLAGS    -> super.fflags   <- v 
    | CSR_FRM       -> super.frm      <- v 
    | CSR_FCSR      -> super.fcsr     <- v 
    | CSR_STATS     -> super.stats    <- v 
    | CSR_SUP0      -> super.sup0     <- v 
    | CSR_SUP1      -> super.sup1     <- v 
    | CSR_EPC       -> super.epc      <- v 
    | CSR_BADVADDR  -> super.badvaddr <- v 
    | CSR_PTBR      -> super.ptbr     <- v 
    | CSR_ASID      -> super.asid     <- v 
    | CSR_COUNT     -> super.count    <- v 
    | CSR_COMPARE   -> super.compare  <- v 
    | CSR_EVEC      -> super.evec     <- v 
    | CSR_CAUSE     -> super.cause    <- v 
    | CSR_STATUS    -> super.status   <- v 
    | CSR_HARTID    -> super.hartid   <- v 
    | CSR_IMPL      -> super.impl     <- v 
    | CSR_FATC      -> super.fatc     <- v 
    | CSR_SEND_IPI  -> super.send_ipi <- v 
    | CSR_CLEAR_IPI -> super.clear_ipi<- v
    | CSR_RESET     -> super.reset    <- v 
    | CSR_TOHOST    -> super.tohost   <- v 
    | CSR_FROMHOST  -> super.fromhost <- v 
    | CSR_CYCLE     -> super.cycle    <- v 
    | CSR_TIME      -> super.time     <- v 
    | CSR_INSTRET   -> super.instret  <- v 
    | CSR_UARCH0    -> super.uarch0   <- v 
    | CSR_UARCH1    -> super.uarch1   <- v 
    | CSR_UARCH2    -> super.uarch2   <- v 
    | CSR_UARCH3    -> super.uarch3   <- v 
    | CSR_UARCH4    -> super.uarch4   <- v 
    | CSR_UARCH5    -> super.uarch5   <- v 
    | CSR_UARCH6    -> super.uarch6   <- v 
    | CSR_UARCH7    -> super.uarch7   <- v 
    | CSR_UARCH8    -> super.uarch8   <- v 
    | CSR_UARCH9    -> super.uarch9   <- v 
    | CSR_UARCH10   -> super.uarch10  <- v 
    | CSR_UARCH11   -> super.uarch11  <- v 
    | CSR_UARCH12   -> super.uarch12  <- v 
    | CSR_UARCH13   -> super.uarch13  <- v 
    | CSR_UARCH14   -> super.uarch14  <- v 
    | CSR_UARCH15   -> super.uarch15  <- v 
    | CSR_COUNTH    -> super.counth   <- v 
    | CSR_CYCLEH    -> super.cycleh   <- v 
    | CSR_TIMEH     -> super.timeh    <- v 
    | CSR_INSTRETH  -> super.instreth <- v 

  let get super r = 
    match r with
    | CSR_FFLAGS    -> super.fflags   
    | CSR_FRM       -> super.frm      
    | CSR_FCSR      -> super.fcsr     
    | CSR_STATS     -> super.stats    
    | CSR_SUP0      -> super.sup0     
    | CSR_SUP1      -> super.sup1     
    | CSR_EPC       -> super.epc      
    | CSR_BADVADDR  -> super.badvaddr 
    | CSR_PTBR      -> super.ptbr     
    | CSR_ASID      -> super.asid     
    | CSR_COUNT     -> super.count    
    | CSR_COMPARE   -> super.compare  
    | CSR_EVEC      -> super.evec     
    | CSR_CAUSE     -> super.cause    
    | CSR_STATUS    -> super.status   
    | CSR_HARTID    -> super.hartid   
    | CSR_IMPL      -> super.impl     
    | CSR_FATC      -> super.fatc     
    | CSR_SEND_IPI  -> super.send_ipi 
    | CSR_CLEAR_IPI -> super.clear_ipi
    | CSR_RESET     -> super.reset    
    | CSR_TOHOST    -> super.tohost   
    | CSR_FROMHOST  -> super.fromhost 
    | CSR_CYCLE     -> super.cycle    
    | CSR_TIME      -> super.time     
    | CSR_INSTRET   -> super.instret  
    | CSR_UARCH0    -> super.uarch0   
    | CSR_UARCH1    -> super.uarch1   
    | CSR_UARCH2    -> super.uarch2   
    | CSR_UARCH3    -> super.uarch3   
    | CSR_UARCH4    -> super.uarch4   
    | CSR_UARCH5    -> super.uarch5   
    | CSR_UARCH6    -> super.uarch6   
    | CSR_UARCH7    -> super.uarch7   
    | CSR_UARCH8    -> super.uarch8   
    | CSR_UARCH9    -> super.uarch9   
    | CSR_UARCH10   -> super.uarch10  
    | CSR_UARCH11   -> super.uarch11  
    | CSR_UARCH12   -> super.uarch12  
    | CSR_UARCH13   -> super.uarch13  
    | CSR_UARCH14   -> super.uarch14  
    | CSR_UARCH15   -> super.uarch15  
    | CSR_COUNTH    -> super.counth   
    | CSR_CYCLEH    -> super.cycleh   
    | CSR_TIMEH     -> super.timeh    
    | CSR_INSTRETH  -> super.instreth 

end


