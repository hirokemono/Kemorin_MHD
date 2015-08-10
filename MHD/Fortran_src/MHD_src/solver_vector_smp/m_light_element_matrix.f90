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
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
!
      call alloc_type_djds11_mat(node1%numnod, internal_node,           &
     &    DJDS_fluid, Cmat_DJDS)
      call reset_aiccg_composit
!
      end subroutine allocate_aiccg_composit
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_composit
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      integer(kind = kint) :: inod, iele, in, k1
!
      Cmat_DJDS%aiccg = 0.0d0
!
      do inod = 1, node1%numnod
        Cmat_DJDS%aiccg(inod) = 1.0d0
      end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = DJDS_fluid%OLDtoNEW(inod)
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
