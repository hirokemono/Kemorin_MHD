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
       use m_geometry_parameter
       use m_solver_djds_fluid
!
!
      Tmat_DJDS%num_diag =      numnod
      Tmat_DJDS%internal_diag = internal_node
!
       Tmat_DJDS%istart_diag = 1
       Tmat_DJDS%istart_l = numnod + 1
       Tmat_DJDS%istart_u = numnod + itotal_fl_l + 1
!
       Tmat_DJDS%num_non0 = numnod + itotal_fl_u + itotal_fl_l
!
       allocate(Tmat_DJDS%aiccg(0:Tmat_DJDS%num_non0))
!
       allocate (Tmat_DJDS%ALUG_U(internal_node) )
       allocate (Tmat_DJDS%ALUG_L(internal_node) )
!
       Tmat_DJDS%D =>  Tmat_DJDS%aiccg(Tmat_DJDS%istart_diag:Tmat_DJDS%istart_l-1)
       Tmat_DJDS%AL => Tmat_DJDS%aiccg(Tmat_DJDS%istart_l:Tmat_DJDS%istart_u-1)
       Tmat_DJDS%AU => Tmat_DJDS%aiccg(Tmat_DJDS%istart_u:Tmat_DJDS%num_non0)
!
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
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_fluid
!
      integer(kind = kint) :: inod, iele, in, k1
!
!
       Tmat_DJDS%aiccg = 0.0d0
!
       do inod = 1, numnod
        Tmat_DJDS%aiccg(inod) = 1.0d0
       end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = OLDtoNEW(inod)
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
