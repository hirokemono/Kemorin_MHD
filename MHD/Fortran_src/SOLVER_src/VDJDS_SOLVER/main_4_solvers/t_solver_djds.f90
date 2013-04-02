!
!      module t_solver_djds
!
!>  Structer for DJDS matrix solver
!
!      Written by Hiroaki Matsui on Dec., 2008
!
!        (module m_solver_djds)
!
!      subroutine link_alloc_type_djds11_mat(numnod, internal_node,     &
!     &          comm_table, djds_table, solver)
!      subroutine link_alloc_type_djds33_mat(numnod, internal_node,     &
!     &          comm_table, djds_table, solver)
!      subroutine link_alloc_type_djdsNN_mat(numnod, internal_node, NB, &
!     &          comm_table, djds_table, solver)
!      subroutine link_alloc_type_zero_mat(comm_table, djds_table,      &
!     &          solver)
!
!       subroutine alloc_type_4_RCM(numnod, djds_tbl)
!       subroutine alloc_type_number_4_djds(djds_tbl)
!       subroutine alloc_type_lists_4_DJDS(np_smp, numnod, djds_tbl)
!       subroutine alloc_type_address_4_DJDS(djds_tbl)
!       subroutine alloc_type_new_comm_table(ntot_export, djds_tbl)
!       subroutine alooc_djds_zero_connect_type(djds_tbl)
!
!       subroutine alloc_type_djds11_mat(numnod, internal_node,         &
!      &          djds_tbl, mat11)
!       subroutine alloc_type_djds33_mat(numnod, internal_node,         &
!      &          djds_tbl, mat33)
!       subroutine alloc_type_djdsNN_mat(numnod, internal_node, NB,     &
!      &          djds_tbl, matNN)
!       subroutine alloc_type_zero_mat(mat)
!
!       subroutine dealloc_type_4_djds_table(djds_tbl)
!       subroutine dealloc_type_4_RCM(djds_tbl)
!       subroutine dealloc_type_number_4_djds(djds_tbl)
!       subroutine dealloc_type_lists_4_DJDS(djds_tbl)
!       subroutine dealloc_type_address_4_DJDS(djds_tbl)
!       subroutine dealloc_type_new_comm_table(djds_tbl)
!
!       subroutine dealloc_type_djds_mat(mat)
!
!       subroutine check_type_DJDS_ordering_info(my_rank, numnod,       &
!     &           djds_tbl)
!
!      subroutine link_djds_connect_structs(djds_org, djds_tbl)
!      subroutine link_djds_matrix_structs(mat_org, mat)
!      subroutine unlink_djds_connect_structs(djds_tbl)
!
      module t_solver_djds
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
!
!
!>  Structer for matrix ordering
      type DJDS_ordering_table
        integer(kind=kint) :: itotal_l
        integer(kind=kint) :: itotal_u
!
        integer(kind=kint) :: NHYP
        integer(kind=kint), pointer :: IVECT(:)
!
        integer(kind=kint), pointer :: OLDtoNEW(:)
        integer(kind=kint), pointer :: NEWtoOLD(:)
!
        integer(kind=kint), pointer :: NEWtoOLD_DJDS_L(:)
        integer(kind=kint), pointer :: NEWtoOLD_DJDS_U(:)
        integer(kind=kint), pointer :: OLDtoNEW_DJDS_L(:)
        integer(kind=kint), pointer :: OLDtoNEW_DJDS_U(:)
        integer(kind=kint), pointer :: LtoU(:)
!
        integer(kind=kint), pointer :: indexDJDS_L(:)
        integer(kind=kint), pointer :: indexDJDS_U(:)
        integer(kind=kint), pointer :: itemDJDS_L(:)
        integer(kind=kint), pointer :: itemDJDS_U(:)
        integer(kind=kint), pointer :: NLmaxHYP(:)
        integer(kind=kint), pointer :: NUmaxHYP(:)
!
        integer(kind=kint), pointer :: STACKmcG(:)
        integer(kind=kint), pointer :: STACKmc(:)
        integer(kind=kint), pointer :: COLORon(:)
        integer(kind=kint), pointer :: PEon(:)
!
        integer(kind = kint), pointer :: NOD_EXPORT_NEW(:)
!
!  ---  constants
!
        integer(kind=kint) :: NLmax, NUmax
        integer(kind=kint) :: npLX1, npUX1
!
      end type DJDS_ordering_table
!
!
      type DJDS_MATRIX
!>   coefficients of matrix
        real(kind=kreal), pointer :: aiccg(:)
!>   diagonal component of matrix
        real(kind=kreal), pointer :: D(:)
!>   lower off-diagonal of matrix
        real(kind=kreal), pointer :: AL(:)
!>   upper off-diagonal of matrix
        real(kind=kreal), pointer :: AU(:)
!
!>   number of diagonal component ind internal
        integer (kind = kint) :: num_diag, internal_diag
!
!>   pointer for diagonal component
        integer (kind = kint) :: istart_diag
!>   pointer for upper part of matrix
        integer (kind = kint) :: istart_u
!>   pointer for lower part of matrix
        integer (kind = kint) :: istart_l
!>   total number of component
        integer (kind = kint) :: num_comp
!
        real(kind=kreal), pointer :: ALUG_L(:)
        real(kind=kreal), pointer :: ALUG_U(:)
      end type DJDS_MATRIX
!
      type DJDS_SOLVER_ARRAYS
        type(DJDS_ordering_table) :: djds_tbl
        type(DJDS_MATRIX) ::         djds_mat
        type(communication_table) :: djds_comm
      end type DJDS_SOLVER_ARRAYS
!
      integer(kind = kint), parameter, private :: izero = 0
!
! ------------------------------------------
!
      contains
!
! ------------------------------------------
!
      subroutine link_alloc_type_djds11_mat(numnod, internal_node,      &
     &          comm_table, djds_table, solver)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(communication_table), intent(in) :: comm_table
      type(DJDS_ordering_table), intent(in) :: djds_table
      type(DJDS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djds_comm)
      call link_djds_connect_structs(djds_table, solver%djds_tbl)
      call alloc_type_djds11_mat(numnod, internal_node,                 &
     &    solver%djds_tbl, solver%djds_mat)
!
      end subroutine link_alloc_type_djds11_mat
!
! ------------------------------------------
!
      subroutine link_alloc_type_djds33_mat(numnod, internal_node,      &
     &          comm_table, djds_table, solver)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(communication_table), intent(in) :: comm_table
      type(DJDS_ordering_table), intent(in) :: djds_table
      type(DJDS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djds_comm)
      call link_djds_connect_structs(djds_table, solver%djds_tbl)
      call alloc_type_djds33_mat(numnod, internal_node,                 &
     &    solver%djds_tbl, solver%djds_mat)
!
      end subroutine link_alloc_type_djds33_mat
!
! ------------------------------------------
!
      subroutine link_alloc_type_djdsNN_mat(numnod, internal_node, NB,  &
     &          comm_table, djds_table, solver)
!
      integer(kind = kint), intent(in) :: numnod, internal_node, NB
      type(communication_table), intent(in) :: comm_table
      type(DJDS_ordering_table), intent(in) :: djds_table
      type(DJDS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djds_comm)
      call link_djds_connect_structs(djds_table, solver%djds_tbl)
      call alloc_type_djdsNN_mat(numnod, internal_node, NB,             &
     &    solver%djds_tbl, solver%djds_mat)
!
      end subroutine link_alloc_type_djdsNN_mat
!
! ------------------------------------------
!
      subroutine link_alloc_type_zero_mat(comm_table, djds_table,       &
     &          solver)
!
      type(communication_table), intent(in) :: comm_table
      type(DJDS_ordering_table), intent(in) :: djds_table
      type(DJDS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djds_comm)
      call link_djds_connect_structs(djds_table, solver%djds_tbl)
      call alloc_type_zero_mat(solver%djds_mat)
!
      end subroutine link_alloc_type_zero_mat
!
! ------------------------------------------
! ------------------------------------------
!
       subroutine alloc_type_4_RCM(numnod, djds_tbl)
!
       integer(kind = kint), intent(in) :: numnod
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       allocate (djds_tbl%OLDtoNEW(numnod) )
       allocate (djds_tbl%NEWtoOLD(numnod) )

       if ( numnod .gt. 0) then
         djds_tbl%OLDtoNEW= 0
         djds_tbl%NEWtoOLD= 0
       end if
!
       end subroutine alloc_type_4_RCM
!
! ------------------------------------------
!
       subroutine alloc_type_number_4_djds(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       allocate (djds_tbl%IVECT(0:djds_tbl%NHYP) )
       allocate (djds_tbl%NLmaxHYP(djds_tbl%NHYP))
       allocate (djds_tbl%NUmaxHYP(djds_tbl%NHYP))
!
       djds_tbl%IVECT   = 0
       if ( djds_tbl%NHYP .gt. 0) then
         djds_tbl%NLmaxHYP= 0
         djds_tbl%NUmaxHYP= 0
       end if
!
       end subroutine alloc_type_number_4_djds
!
! ------------------------------------------
!
       subroutine alloc_type_lists_4_DJDS(np_smp, numnod, djds_tbl)
!
       integer(kind = kint), intent(in) :: np_smp, numnod
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
       integer(kind = kint) :: num_l, num_u
!
       allocate (djds_tbl%NEWtoOLD_DJDS_L(numnod))
       allocate (djds_tbl%OLDtoNEW_DJDS_L(numnod))
       allocate (djds_tbl%NEWtoOLD_DJDS_U(numnod))
       allocate (djds_tbl%OLDtoNEW_DJDS_U(numnod))
       allocate (djds_tbl%LtoU(numnod))
!
       num_l = np_smp * djds_tbl%NLmax * djds_tbl%NHYP
       num_u = np_smp * djds_tbl%NUmax * djds_tbl%NHYP
       allocate (djds_tbl%indexDJDS_L(0:num_l))
       allocate (djds_tbl%indexDJDS_U(0:num_u))

       allocate (djds_tbl%STACKmcG (0:np_smp) )
       allocate (djds_tbl%STACKmc (0:np_smp*djds_tbl%NHYP) )
!
       allocate (djds_tbl%PEon(numnod))
       allocate (djds_tbl%COLORon(numnod))
!
       djds_tbl%STACKmcG = 0
       djds_tbl%STACKmc = 0
       djds_tbl%indexDJDS_L= 0
       djds_tbl%indexDJDS_U= 0
!
       if( numnod .gt. 0) then
         djds_tbl%NEWtoOLD_DJDS_L = 0
         djds_tbl%NEWtoOLD_DJDS_U = 0
         djds_tbl%OLDtoNEW_DJDS_L = 0
         djds_tbl%OLDtoNEW_DJDS_U = 0
         djds_tbl%LtoU = 0
!
         djds_tbl%PEon = 0
         djds_tbl%COLORon = 0
       end if
!
!
       end subroutine alloc_type_lists_4_DJDS
!
! ------------------------------------------
!
       subroutine alloc_type_address_4_DJDS(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       allocate (djds_tbl%itemDJDS_L(djds_tbl%itotal_l))
       allocate (djds_tbl%itemDJDS_U(djds_tbl%itotal_u))
!
       if (djds_tbl%itotal_l .gt. 0) djds_tbl%itemDJDS_L = 0
       if (djds_tbl%itotal_u .gt. 0) djds_tbl%itemDJDS_U = 0
!
       end subroutine alloc_type_address_4_DJDS
!
! ------------------------------------------
!
       subroutine alloc_type_new_comm_table(ntot_export, djds_tbl)
!
       integer(kind = kint), intent(in) :: ntot_export
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       allocate  ( djds_tbl%NOD_EXPORT_NEW(ntot_export) )
       if (ntot_export .gt. 0) djds_tbl%NOD_EXPORT_NEW = 0
!
       end subroutine alloc_type_new_comm_table
!
! ------------------------------------------
!
      subroutine alooc_djds_zero_connect_type(djds_tbl)
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = izero
      djds_tbl%itotal_u = izero
      djds_tbl%NHYP =     izero
      djds_tbl%NLmax =    izero
      djds_tbl%NUmax =    izero
      djds_tbl%npLX1 =    izero
      djds_tbl%npUX1 =    izero
!
      call alloc_type_4_RCM(izero, djds_tbl)
      call alloc_type_number_4_djds(djds_tbl)
      call alloc_type_lists_4_DJDS(izero, izero, djds_tbl)
      call alloc_type_address_4_DJDS(djds_tbl)
      call alloc_type_new_comm_table(izero, djds_tbl)
!
      end subroutine alooc_djds_zero_connect_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_type_djds11_mat(numnod, internal_node,           &
     &          djds_tbl, mat11)
!
       integer(kind = kint), intent(in) :: numnod, internal_node
       type(DJDS_ordering_table), intent(in) :: djds_tbl
       type(DJDS_MATRIX), intent(inout) :: mat11
!
!
       mat11%num_diag =      numnod
       mat11%internal_diag = internal_node
!
       mat11%istart_diag = 1
       mat11%istart_l = numnod + 1
       mat11%istart_u = numnod + djds_tbl%itotal_l + 1
!
       mat11%num_comp = numnod + djds_tbl%itotal_l + djds_tbl%itotal_u
!
       allocate(mat11%aiccg(0:mat11%num_comp) )
       allocate(mat11%ALUG_U(internal_node)  )
       allocate(mat11%ALUG_L(internal_node)  )
       if(mat11%num_comp .gt. 0) mat11%aiccg =  0.0d0
       if(internal_node .gt. 0)  mat11%ALUG_U = 0.0d0
       if(internal_node .gt. 0)  mat11%ALUG_L = 0.0d0
!
       mat11%D =>  mat11%aiccg(mat11%istart_diag:mat11%istart_l-1)
       mat11%AL => mat11%aiccg(mat11%istart_l:mat11%istart_u-1)
       mat11%AU => mat11%aiccg(mat11%istart_u:mat11%num_comp)
!
       end subroutine alloc_type_djds11_mat
!
! ------------------------------------------
!
      subroutine alloc_type_djds33_mat(numnod, internal_node,           &
     &          djds_tbl, mat33)
!
       integer(kind = kint), intent(in) :: numnod, internal_node
       type(DJDS_ordering_table), intent(in) :: djds_tbl
       type(DJDS_MATRIX), intent(inout) :: mat33
!
!
       mat33%num_diag =      numnod
       mat33%internal_diag = internal_node
!
       mat33%istart_diag = 1
       mat33%istart_l = 9*numnod + 1
       mat33%istart_u = 9*(numnod + djds_tbl%itotal_l) + 1
!
       mat33%num_comp = 9 * (numnod + djds_tbl%itotal_l                 &
     &                              + djds_tbl%itotal_u)
!
       allocate(mat33%aiccg(-8:mat33%num_comp) )
       allocate(mat33%ALUG_U(9*internal_node)  )
       allocate(mat33%ALUG_L(9*internal_node)  )
       if(mat33%num_comp .gt. 0) mat33%aiccg = 0.0d0
       if(internal_node .gt. 0)  mat33%ALUG_U = 0.0d0
       if(internal_node .gt. 0)  mat33%ALUG_L = 0.0d0
!
       mat33%D =>  mat33%aiccg(mat33%istart_diag:mat33%istart_l-1)
       mat33%AL => mat33%aiccg(mat33%istart_l:mat33%istart_u-1)
       mat33%AU => mat33%aiccg(mat33%istart_u:mat33%num_comp)
!
       end subroutine alloc_type_djds33_mat
!
! ------------------------------------------
!
      subroutine alloc_type_djdsNN_mat(numnod, internal_node, NB,       &
     &          djds_tbl, matNN)
!
       integer(kind = kint), intent(in) :: numnod, internal_node, NB
       type(DJDS_ordering_table), intent(in) :: djds_tbl
       type(DJDS_MATRIX), intent(inout) :: matNN
       integer(kind = kint) :: NB2
!
!
       matNN%num_diag =      numnod
       matNN%internal_diag = internal_node
!
       NB2= NB*NB
       matNN%istart_diag = 1
       matNN%istart_l = NB2*numnod + 1
       matNN%istart_u = NB2*(numnod + djds_tbl%itotal_l) + 1
!
       matNN%num_comp = NB2 * (numnod + djds_tbl%itotal_l               &
     &                                  + djds_tbl%itotal_u)
!
       allocate(matNN%aiccg(-NB2+1:matNN%num_comp) )
       allocate(matNN%ALUG_U(NB2*internal_node)  )
       allocate(matNN%ALUG_L(NB2*internal_node)  )
       if(matNN%num_comp .gt. 0) matNN%aiccg = 0.0d0
       if(internal_node .gt. 0)  matNN%ALUG_L = 0.0d0
       if(internal_node .gt. 0)  matNN%ALUG_U = 0.0d0
!
       matNN%D =>  matNN%aiccg(matNN%istart_diag:matNN%istart_l-1)
       matNN%AL => matNN%aiccg(matNN%istart_l:matNN%istart_u-1)
       matNN%AU => matNN%aiccg(matNN%istart_u:matNN%num_comp)
!
       end subroutine alloc_type_djdsNN_mat
!
! ------------------------------------------
!
       subroutine alloc_type_zero_mat(mat)
!
       type(DJDS_MATRIX), intent(inout) :: mat
!
!
       mat%num_diag =      izero
       mat%internal_diag = izero
!
       mat%istart_diag = izero
       mat%istart_u =    izero
       mat%istart_l =    izero
!
       mat%num_comp = izero
!
       allocate(mat%aiccg(izero:izero) )
       allocate(mat%ALUG_U(izero)  )
       allocate(mat%ALUG_L(izero)  )
       mat%aiccg(izero) = 0.0d0
!
       mat%D =>  mat%aiccg(izero:izero)
       mat%AU => mat%aiccg(izero:izero)
       mat%AL => mat%aiccg(izero:izero)
!
       end subroutine alloc_type_zero_mat
!
! ------------------------------------------
! ------------------------------------------
!
       subroutine dealloc_type_4_djds_table(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       deallocate (djds_tbl%NEWtoOLD_DJDS_L)
!
       end subroutine dealloc_type_4_djds_table
!
! ------------------------------------------
!
       subroutine dealloc_type_4_RCM(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       deallocate (djds_tbl%OLDtoNEW )
       deallocate (djds_tbl%NEWtoOLD )
!
       end subroutine dealloc_type_4_RCM
!
! ------------------------------------------
!
       subroutine dealloc_type_number_4_djds(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       deallocate (djds_tbl%IVECT)
       deallocate (djds_tbl%NLmaxHYP)
       deallocate (djds_tbl%NUmaxHYP)
!
       end subroutine dealloc_type_number_4_djds
!
! ------------------------------------------
!
       subroutine dealloc_type_lists_4_DJDS(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       deallocate (djds_tbl%OLDtoNEW_DJDS_L)
       deallocate (djds_tbl%NEWtoOLD_DJDS_U)
       deallocate (djds_tbl%OLDtoNEW_DJDS_U)
       deallocate (djds_tbl%LtoU)

       deallocate (djds_tbl%indexDJDS_L)
       deallocate (djds_tbl%indexDJDS_U)

       deallocate (djds_tbl%STACKmcG)
       deallocate (djds_tbl%STACKmc)
!
       deallocate (djds_tbl%PEon)
       deallocate (djds_tbl%COLORon)
!
       end subroutine dealloc_type_lists_4_DJDS
!
! ------------------------------------------
!
       subroutine dealloc_type_address_4_DJDS(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       deallocate (djds_tbl%itemDJDS_L)
       deallocate (djds_tbl%itemDJDS_U)
!
       end subroutine dealloc_type_address_4_DJDS
!
! ------------------------------------------
!
       subroutine dealloc_type_new_comm_table(djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
       deallocate  ( djds_tbl%NOD_EXPORT_NEW )
!
       end subroutine dealloc_type_new_comm_table
!
! ------------------------------------------
! ------------------------------------------
!
       subroutine dealloc_type_djds_mat(mat)
!
       type(DJDS_MATRIX), intent(inout) :: mat
!
!
       nullify(mat%D, mat%AL, mat%AU)
!
       deallocate(mat%aiccg)
       deallocate(mat%ALUG_U)
       deallocate(mat%ALUG_L)
!
       end subroutine dealloc_type_djds_mat
!
! ------------------------------------------
!
      subroutine link_djds_connect_structs(djds_org, djds_tbl)
!
      type(DJDS_ordering_table), intent(in) ::    djds_org
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = djds_org%itotal_l
      djds_tbl%itotal_u = djds_org%itotal_u
      djds_tbl%NHYP =     djds_org%NHYP
      djds_tbl%NLmax =    djds_org%NLmax
      djds_tbl%NUmax =    djds_org%NUmax
      djds_tbl%npLX1 =    djds_org%npLX1
      djds_tbl%npUX1 =    djds_org%npUX1
!
      djds_tbl%IVECT =>           djds_org%IVECT
      djds_tbl%NEWtoOLD =>        djds_org%NEWtoOLD
      djds_tbl%OLDtoNEW =>        djds_org%OLDtoNEW
      djds_tbl%NEWtoOLD_DJDS_U => djds_org%NEWtoOLD_DJDS_U
      djds_tbl%OLDtoNEW_DJDS_L => djds_org%OLDtoNEW_DJDS_L
      djds_tbl%OLDtoNEW_DJDS_U => djds_org%OLDtoNEW_DJDS_U
      djds_tbl%LtoU =>            djds_org%LtoU
      djds_tbl%indexDJDS_L =>     djds_org%indexDJDS_L
      djds_tbl%indexDJDS_U =>     djds_org%indexDJDS_U
      djds_tbl%itemDJDS_L =>      djds_org%itemDJDS_L
      djds_tbl%itemDJDS_U =>      djds_org%itemDJDS_U
      djds_tbl%NLmaxHYP =>        djds_org%NLmaxHYP
      djds_tbl%NUmaxHYP =>        djds_org%NUmaxHYP
      djds_tbl%STACKmcG =>        djds_org%STACKmcG
      djds_tbl%STACKmc =>         djds_org%STACKmc
!
      djds_tbl%NOD_EXPORT_NEW =>  djds_org%NOD_EXPORT_NEW
!
      end subroutine link_djds_connect_structs
!
! ------------------------------------------
!
      subroutine link_djds_matrix_structs(mat_org, mat)
!
      type(DJDS_MATRIX), intent(inout) :: mat
      type(DJDS_MATRIX), intent(inout) :: mat_org
!
!
      mat%num_diag =      mat_org%num_diag
      mat%internal_diag = mat_org%internal_diag
      mat%istart_diag =   mat_org%istart_diag
      mat%istart_l =      mat_org%istart_l
      mat%istart_u =      mat_org%istart_u
      mat%num_comp =      mat_org%num_comp
!
      mat%aiccg =>  mat_org%aiccg
      mat%ALUG_L => mat_org%ALUG_L
      mat%ALUG_U => mat_org%ALUG_U
!
      end subroutine link_djds_matrix_structs
!
! ------------------------------------------
!
      subroutine unlink_djds_connect_structs(djds_tbl)
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = 0
      djds_tbl%itotal_u = 0
      djds_tbl%NHYP =     0
      djds_tbl%NLmax =    0
      djds_tbl%NUmax =    0
      djds_tbl%npLX1 =    0
      djds_tbl%npUX1 =    0
!
      nullify( djds_tbl%IVECT )
      nullify( djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW)
      nullify( djds_tbl%NEWtoOLD_DJDS_U, djds_tbl%LtoU )
      nullify( djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U)
      nullify( djds_tbl%indexDJDS_L, djds_tbl%itemDJDS_L )
      nullify( djds_tbl%indexDJDS_U, djds_tbl%itemDJDS_U )
      nullify( djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP )
      nullify( djds_tbl%STACKmcG, djds_tbl%STACKmc )
!
      nullify( djds_tbl%NOD_EXPORT_NEW )
!
      end subroutine unlink_djds_connect_structs
!
! ------------------------------------------
! ------------------------------------------
!
       subroutine check_type_DJDS_ordering_info(my_rank, numnod,        &
     &           djds_tbl)
!
       integer (kind = kint), intent(in) :: my_rank, numnod
       type(DJDS_ordering_table), intent(in) :: djds_tbl
!
       integer(kind = kint) :: i
!
      write(50+my_rank,*) 'inod, NEWtoOLD, OLDtoNEW_DJDS_L, ',          &
     &        ' OLDtoNEW_DJDS_U, LtoU'
      do i = 1, numnod
      write(50+my_rank,'(10i8)') i, djds_tbl%NEWtoOLD(i),               &
     &                              djds_tbl%OLDtoNEW_DJDS_L(i),        &
     &                              djds_tbl%OLDtoNEW_DJDS_U(i),        &
     &                              djds_tbl%LtoU(i)
      end do
!
      write(50+my_rank,*) 'indexDJDS_L'
      write(50+my_rank,'(10i8)') djds_tbl%indexDJDS_L
      write(50+my_rank,*) 'itemDJDS_l'
      write(50+my_rank,'(10i8)') djds_tbl%itemDJDS_l
      write(50+my_rank,*) 'indexDJDS_U'
      write(50+my_rank,'(10i8)') djds_tbl%indexDJDS_U
      write(50+my_rank,*) 'itemDJDS_u'
      write(50+my_rank,'(10i8)') djds_tbl%itemDJDS_u
!
      end subroutine check_type_DJDS_ordering_info
!
! ------------------------------------------
!
      end module t_solver_djds
