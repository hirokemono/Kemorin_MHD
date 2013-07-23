
!
!     module   m_magne_matrix
!.......................................................................
!
!     Written by H. Matsui
!
!       subroutine allocate_aiccg_magne
!       subroutine reset_aiccg_magne
!       subroutine deallocate_aiccg_magne
!
      module   m_magne_matrix
!
      use m_precision
!
      implicit  none
!
!
      real(kind=kreal), allocatable, target :: aiccg_magne(:)
!   coefficients of matrix
!
      integer (kind = kint) :: im_mag_d
!   pointer for diagonal component
      integer (kind = kint) :: im_mag_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_mag_l
!   pointer for lower part of matrix
      integer (kind = kint) :: num_mag_comp
!   total number of component
!
      real(kind=kreal), allocatable, target :: ALUG_magne_L(:)
      real(kind=kreal), allocatable, target :: ALUG_magne_U(:)
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
        im_mag_d = 1
        im_mag_l = 9*numnod + 1
        im_mag_u = 9*(numnod+itotal_l) + 1
!
        num_mag_comp = 9 * (numnod+itotal_u+itotal_l)
!
        allocate(aiccg_magne(-8:num_mag_comp) )
!
        allocate (ALUG_magne_U(9*internal_node) )
        allocate (ALUG_magne_L(9*internal_node) )
!
        call reset_aiccg_magne
!
      end subroutine allocate_aiccg_magne
!
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
      aiccg_magne = 0.0d0
!
      if (iflag_t_evo_4_magne .ge. 3) then
        do inod = 1, numnod
          aiccg_magne(9*inod-8) = 1.0d0
          aiccg_magne(9*inod-4) = 1.0d0
          aiccg_magne(9*inod  ) = 1.0d0
        end do
!
        do k1 = 1, nnod_4_ele
          do iele = iele_cd_start, iele_cd_end
            inod = ie(iele,k1)
            in = OLDtoNEW(inod)
            aiccg_magne(9*in-8) = 0.0d0
            aiccg_magne(9*in-4) = 0.0d0
            aiccg_magne(9*in  ) = 0.0d0
          end do
        end do
      end if
!
      ALUG_magne_U= 1.d0
      ALUG_magne_L= 1.d0
!
      end subroutine reset_aiccg_magne
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_magne
!
!
        deallocate(aiccg_magne)
!
        deallocate (ALUG_magne_U)
        deallocate (ALUG_magne_L)
!
      end subroutine deallocate_aiccg_magne
!
! ----------------------------------------------------------------------
!
      end module m_magne_matrix
!
