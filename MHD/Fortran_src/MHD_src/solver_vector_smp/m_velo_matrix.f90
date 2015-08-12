!>@file   m_velo_matrix.f90
!!@brief  module m_velo_matrix
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS matrix for velocity
!!
!!@verbatim
!!      subroutine allocate_aiccg_velo
!!      subroutine deallocate_aiccg_velo
!!      subroutine reset_aiccg_velo
!!
!!       subroutine allocate_aiccg_press
!!       subroutine reset_aiccg_press
!!       subroutine deallocate_aiccg_press
!!@endverbatim
!
      module m_velo_matrix
!
      use m_precision
      use t_solver_djds
!
      implicit  none
!
!
!>      Structure of matrix for time evolution of velocity
      type(DJDS_MATRIX), save :: Vmat_DJDS
!>      Structure of matrix for poission equation of pressure
      type(DJDS_MATRIX), save :: Pmat_DJDS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_velo
!
      use m_geometry_data
      use m_solver_djds_MHD
!
!
      call alloc_type_djds33_mat(node1%numnod, internal_node,           &
     &    DJDS_fluid, Vmat_DJDS)
      call reset_aiccg_velo
!
      end subroutine allocate_aiccg_velo
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_press
!
      use m_geometry_data
      use m_solver_djds_MHD
!
!
      call alloc_type_djds11_mat(node1%numnod, internal_node,           &
     &    DJDS_fl_l, Pmat_DJDS)
      call reset_aiccg_press
!
      end subroutine allocate_aiccg_press
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_velo
!
!
       call dealloc_type_djds_mat(Vmat_DJDS)
!
       end subroutine deallocate_aiccg_velo
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_press
!
!
       call dealloc_type_djds_mat(Pmat_DJDS)
!
       end subroutine deallocate_aiccg_press
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_velo
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      integer (kind = kint) :: inod, iele, k1, in
!
!
      Vmat_DJDS%aiccg = 0.0d00
!
      do inod = 1, node1%numnod
        Vmat_DJDS%aiccg(inod*9-8) = 1.0d0
        Vmat_DJDS%aiccg(inod*9-4) = 1.0d0
        Vmat_DJDS%aiccg(inod*9  ) = 1.0d0
      end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = DJDS_fluid%OLDtoNEW(inod)
          Vmat_DJDS%aiccg(in*9-8) = 0.0d0
          Vmat_DJDS%aiccg(in*9-4) = 0.0d0
          Vmat_DJDS%aiccg(in*9  ) = 0.0d0
        end do
      end do
!
      Vmat_DJDS%ALUG_U= 1.d0
      Vmat_DJDS%ALUG_L= 1.d0
!
      end subroutine reset_aiccg_velo
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_press
!
      use m_geometry_constants
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      integer(kind = kint) :: inod, iele, in, k1
!
!
      Pmat_DJDS%aiccg = 0.0d0
!
      do inod = 1, node1%numnod
        Pmat_DJDS%aiccg(inod) = 1.0d0
      end do
!
      do k1 = 1, num_t_linear
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = DJDS_fl_l%OLDtoNEW(inod)
          Pmat_DJDS%aiccg(in) = 0.0d0
        end do
      end do
!
      Pmat_DJDS%ALUG_U= 1.d0
      Pmat_DJDS%ALUG_L= 1.d0
!
      end subroutine reset_aiccg_press
!
! ----------------------------------------------------------------------
!
      end module m_velo_matrix
