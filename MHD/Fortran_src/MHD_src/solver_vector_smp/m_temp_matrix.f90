!>@file   m_temp_matrix.f90
!!@brief  module m_temp_matrix
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS matrix for temperature
!!
!!@verbatim
!!       subroutine allocate_aiccg_temp
!!       subroutine reset_aiccg_temp
!!       subroutine deallocate_aiccg_temp
!!@endverbatim
!
      module   m_temp_matrix
!
      use m_precision
      use t_solver_djds
!
      implicit  none
!
!>      Structure of matrix for time evolution of temperature
      type(DJDS_MATRIX), save :: Tmat_DJDS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_temp
!
      use m_geometry_data
      use m_solver_djds_MHD
!
!
      call alloc_type_djds11_mat(node1%numnod, node1%internal_node,     &
     &    DJDS_fluid, Tmat_DJDS)
      call reset_aiccg_temp
!
      end subroutine allocate_aiccg_temp
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_temp
!
!
       call dealloc_type_djds_mat(Tmat_DJDS)
!
       end subroutine deallocate_aiccg_temp
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_temp
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      integer(kind = kint) :: inod, iele, in, k1
!
!
       Tmat_DJDS%aiccg = 0.0d0
!
       do inod = 1, node1%numnod
        Tmat_DJDS%aiccg(inod) = 1.0d0
       end do
!
      do k1 = 1, ele1%nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ele1%ie(iele,k1)
          in = DJDS_fluid%OLDtoNEW(inod)
          Tmat_DJDS%aiccg(in) = 0.0d0
        end do
      end do
!
       Tmat_DJDS%ALUG_U= 0.d0
       Tmat_DJDS%ALUG_L= 0.d0
!
       end subroutine reset_aiccg_temp
!
! ----------------------------------------------------------------------
!
      end module m_temp_matrix
!
