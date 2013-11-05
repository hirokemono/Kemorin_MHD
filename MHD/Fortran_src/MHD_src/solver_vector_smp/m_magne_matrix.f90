!>@file   m_magne_matrix.f90
!!@brief  module m_magne_matrix
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
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
        use m_geometry_parameter
        use m_solver_djds
!
!
       Bmat_DJDS%num_diag =      numnod
       Bmat_DJDS%internal_diag = internal_node
!
       Bmat_DJDS%istart_diag = 1
       Bmat_DJDS%istart_l =    9*numnod + 1
       Bmat_DJDS%istart_u =    9*(numnod + itotal_l) + 1
!
       Bmat_DJDS%num_non0 =    9 * (numnod + itotal_u + itotal_l)
!
!
        allocate(Bmat_DJDS%aiccg(-8:Bmat_DJDS%num_non0) )
!
        allocate (Bmat_DJDS%ALUG_U(9*internal_node) )
        allocate (Bmat_DJDS%ALUG_L(9*internal_node) )
!
       Bmat_DJDS%D =>  Bmat_DJDS%aiccg(Bmat_DJDS%istart_diag:Bmat_DJDS%istart_l-1)
       Bmat_DJDS%AL => Bmat_DJDS%aiccg(Bmat_DJDS%istart_l:Bmat_DJDS%istart_u-1)
       Bmat_DJDS%AU => Bmat_DJDS%aiccg(Bmat_DJDS%istart_u:Bmat_DJDS%num_non0)
!
        call reset_aiccg_magne
!
      end subroutine allocate_aiccg_magne
!
! ----------------------------------------------------------------------
!
       subroutine allocate_aiccg_mag_p
!
       use m_geometry_parameter
       use m_geometry_data
       use m_geometry_data_MHD
       use m_solver_djds_linear
!
!
       Fmat_DJDS%num_diag =      numnod
       Fmat_DJDS%internal_diag = internal_node
!
       Fmat_DJDS%istart_diag = 1
       Fmat_DJDS%istart_l = numnod + 1
       Fmat_DJDS%istart_u = numnod + itotal1_l + 1
!
       Fmat_DJDS%num_non0 = numnod + itotal1_u + itotal1_l
!
       allocate(Fmat_DJDS%aiccg(0:Fmat_DJDS%num_non0) )
!
       allocate (Fmat_DJDS%ALUG_U(internal_node) )
       allocate (Fmat_DJDS%ALUG_L(internal_node) )
!
       Fmat_DJDS%D =>  Fmat_DJDS%aiccg(Fmat_DJDS%istart_diag:Fmat_DJDS%istart_l-1)
       Fmat_DJDS%AL => Fmat_DJDS%aiccg(Fmat_DJDS%istart_l:Fmat_DJDS%istart_u-1)
       Fmat_DJDS%AU => Fmat_DJDS%aiccg(Fmat_DJDS%istart_u:Fmat_DJDS%num_non0)
!
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
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds
!
      integer (kind=kint) :: in, inod, iele, k1
!
!
      Bmat_DJDS%aiccg = 0.0d0
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        do inod = 1, numnod
          Bmat_DJDS%aiccg(9*inod-8) = 1.0d0
          Bmat_DJDS%aiccg(9*inod-4) = 1.0d0
          Bmat_DJDS%aiccg(9*inod  ) = 1.0d0
        end do
!
        do k1 = 1, nnod_4_ele
          do iele = iele_cd_start, iele_cd_end
            inod = ie(iele,k1)
            in = OLDtoNEW(inod)
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
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_linear
!
      integer (kind = kint) :: in, inod, iele, k1
!
      Fmat_DJDS%aiccg = 0.0d0
!
      do inod = 1, numnod
        Fmat_DJDS%aiccg(inod) = 1.0d0
      end do
!
      do k1 = 1, num_t_linear
        do iele = 1, numele
          inod = ie(iele,k1)
          in = OLDtoNEW1(inod)
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
