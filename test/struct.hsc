{{ struct
    struct.h,
    some_nums,
    a, CInt,
    b, CLong,
    c, CFloat,
}}

!!Unimplemented: Anonymous struct!!

{{ struct
    struct.h,
    outer,
    x, Ptr (),
!!Unimplemented Record Field: Nested RecordDecl!!
    inner, !!Unimplemented struct type: struct (unnamed struct at struct.h:18:3)!!,
}}

{{ struct
    struct.h,
    son,
!!Unimplemented Record Field: Nested RecordDecl!!
    mom, !!Unimplemented struct type: struct (unnamed struct at struct.h:25:3)!!,
!!Unimplemented Record Field: Nested RecordDecl!!
    dad, !!Unimplemented struct type: struct (unnamed struct at struct.h:33:3)!!,
}}


