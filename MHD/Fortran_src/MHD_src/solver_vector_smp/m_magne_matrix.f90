!>@file   m_magne_matrix.f90
!!@brief  module m_magne_matrix
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@n Modified in Nov., 2013
!
!>     DJDS matrix for velocity
!!
!!@verbatim
!!       subroutine allocate_aiccg_magne
!!       subroutine reset_aiccg_magne
!!       subroutine deallocate_aiccg_magne
!!
!!       subroutine allocate_aiccg_mag_p
!!       subroutine reset_aiccg_mag_p
!!       subroutine deallocate_aiccg_mag_p
!!@endverbatim
!
      module m_magne_matrix
!
      use m_precision
      use t_solver_djds
!
      implicit  none
!
!>      Structure of matrix for time evolution of magnetic field
      type(DJDS_MATRIX), save :: Bmat_DJDS
!>      Structure of matrix for poission equation of electric potential
      type(DJDS_MATRIX), save :: Fmat_DJDS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_magne
!
      use m_geometry_data
      use m_solver_djds_MHD
!
!
      call alloc_type_djds33_mat(node1%numnod, node1%internal_node,     &
     &    DJDS_entire, Bmat_DJDS)
      call reset_aiccg_magne
!
      end subroutine allocate_aiccg_magne
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_mag_p
!
      use m_geometry_data
      use m_solver_djds_MHD
!
!
      call alloc_type_djds11_mat(node1%numnod, node1%internal_node,     &
     &    DJDS_linear, Fmat_DJDS)
      call reset_aiccg_mag_p
!
      end subroutine allocate_aiccg_mag_p
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_magne
!
!
      call dealloc_type_djds_mat(Bmat_DJDS)
!
      end subroutine deallocate_aiccg_magne
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_mag_p
!
!
      call dealloc_type_djds_mat(Fmat_DJDS)
!
      end subroutine deallocate_aiccg_mag_p
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_magne
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      integer (kind=kint) :: in, inod, iele, k1
!
!
      Bmat_DJDS%aiccg = 0.0d0
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        do inod = 1, node1%numnod
          Bmat_DJDS%aiccg(9*inod-8) = 1.0d0
          Bmat_DJDS%aiccg(9*inod-4) = 1.0d0
          Bmat_DJDS%aiccg(9*inod  ) = 1.0d0
        end do
!
        do k1 = 1, ele1%nnod_4_ele
          do iele = iele_cd_start, iele_cd_end
            inod = ele1%ie(iele,k1)
            in = DJDS_entire%OLDtoNEW(inod)
            Bmat_DJDS%aiccg(9*in-8) = 0.0d0
            Bmat_DJDS%aiccg(9*in-4) = 0.0d0
            Bmat_DJDS%aiccg(9*in  ) = 0.0d0
          end do
        end do
      end if
!
      Bmat_DJDS%ALUG_U= 1.d0
      Bmat_DJDS%ALUG_L= 1.d0
!
      end subroutine reset_aiccg_magne
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_mag_p
!
      use m_geometry_constants
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      integer (kind = kint) :: in, inod, iele, k1
!
      Fmat_DJDS%aiccg = 0.0d0
!
      do inod = 1, node1%numnod
        Fmat_DJDS%aiccg(inod) = 1.0d0
      end do
!
      do k1 = 1, num_t_linear
        do iele = 1, ele1%numele
          inod = ele1%ie(iele,k1)
          in = DJDS_linear%OLDtoNEW(inod)
          Fmat_DJDS%aiccg(in) = 0.0d0
        end do
      end do
!
      Fmat_DJDS%ALUG_U= 1.d0
      Fmat_DJDS%ALUG_L= 1.d0
!
      end subroutine reset_aiccg_mag_p
!
! ----------------------------------------------------------------------
!
      end module m_magne_matrix
