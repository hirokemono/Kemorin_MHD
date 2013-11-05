!>@file   m_light_element_matrix.f90
!!@brief  module m_light_element_matrix
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS matrix for light element
!!
!!@verbatim
!!       subroutine allocate_aiccg_composit
!!       subroutine reset_aiccg_composit
!!       subroutine deallocate_aiccg_composit
!!@endverbatim
!
      module   m_light_element_matrix
!
      use m_precision
      use t_solver_djds
!
      implicit  none
!
!
!>      Structure of matrix for time evolution of conposition variation
      type(DJDS_MATRIX), save :: Cmat_DJDS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_aiccg_composit
!
       use m_geometry_parameter
       use m_geometry_data_MHD
       use m_solver_djds_fluid
!
      Cmat_DJDS%num_diag =      numnod
      Cmat_DJDS%internal_diag = internal_node
!
       Cmat_DJDS%istart_diag = 1
       Cmat_DJDS%istart_l = numnod + 1
       Cmat_DJDS%istart_u = numnod + itotal_fl_l + 1
!
       Cmat_DJDS%num_non0 = numnod + itotal_fl_u + itotal_fl_l
!
       allocate(Cmat_DJDS%aiccg(0:Cmat_DJDS%num_non0))
!
       allocate (Cmat_DJDS%ALUG_U(internal_node) )
       allocate (Cmat_DJDS%ALUG_L(internal_node) )
!
       Cmat_DJDS%D =>  Cmat_DJDS%aiccg(Cmat_DJDS%istart_diag:Cmat_DJDS%istart_l-1)
       Cmat_DJDS%AL => Cmat_DJDS%aiccg(Cmat_DJDS%istart_l:Cmat_DJDS%istart_u-1)
       Cmat_DJDS%AU => Cmat_DJDS%aiccg(Cmat_DJDS%istart_u:Cmat_DJDS%num_non0)
!
       call reset_aiccg_composit
!
       end subroutine allocate_aiccg_composit
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_composit
!
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_fluid
!
      integer(kind = kint) :: inod, iele, in, k1
!
      Cmat_DJDS%aiccg = 0.0d0
!
      do inod = 1, numnod
        Cmat_DJDS%aiccg(inod) = 1.0d0
      end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = OLDtoNEW(inod)
          Cmat_DJDS%aiccg(in) = 0.0d0
        end do
      end do
!
       Cmat_DJDS%ALUG_U= 0.d0
       Cmat_DJDS%ALUG_L= 0.d0
!
       end subroutine reset_aiccg_composit
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_composit
!
!
       deallocate(Cmat_DJDS%aiccg)
!
       deallocate (Cmat_DJDS%ALUG_U)
       deallocate (Cmat_DJDS%ALUG_L)
!
       end subroutine deallocate_aiccg_composit
!
! ----------------------------------------------------------------------
!
      end module m_light_element_matrix
