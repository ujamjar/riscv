module Make(D : Types.D) : sig
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
  val to_addr : e -> int
  val of_addr : int -> e
  val get : t -> e -> D.t
  val set : t -> e -> D.t -> unit
  val init : unit -> t
  
  module Status : sig
    val s : int * int
    val ps : int * int
    val ei : int * int
    val pei : int * int
    val ef : int * int
    val u64 : int * int
    val s64 : int * int
    val vm : int * int
    val ea : int * int
    val im : int * int
    val ip : int * int
    val set : (int * int) -> D.t -> D.t
    val get : (int * int) -> D.t -> D.t
  end

end

