!
!     module   m_temp_matrix
!.......................................................................
!
!      Written by H. Matsui
!
      module   m_temp_matrix
!
      use m_precision
!
      implicit  none
!
!
!
      real(kind=kreal), allocatable, target :: aiccg_temp(:)
!   coefficients of matrix
!
      integer (kind = kint) :: im_temp_d
!   pointer for diagonal component
      integer (kind = kint) :: im_temp_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_temp_l
!   pointer for lower part of matrix
      integer (kind = kint) :: num_temp_comp
!   total number of component
!
      real(kind=kreal), allocatable, target :: ALUG_temp_u(:)
      real(kind=kreal), allocatable, target :: ALUG_temp_l(:)
!
!       subroutine allocate_aiccg_temp
!       subroutine reset_aiccg_temp
!       subroutine deallocate_aiccg_temp
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
       im_temp_d = 1
       im_temp_l = numnod + 1
       im_temp_u = numnod + itotal_fl_l + 1
!
       num_temp_comp = numnod+itotal_fl_u+itotal_fl_l
!
       allocate(aiccg_temp(0:num_temp_comp))
!
       allocate (ALUG_temp_U(internal_node) )
       allocate (ALUG_temp_L(internal_node) )
!
       call reset_aiccg_temp
!
       end subroutine allocate_aiccg_temp
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
       aiccg_temp = 0.0d0
!
       do inod = 1, numnod
        aiccg_temp(inod) = 1.0d0
       end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = OLDtoNEW(inod)
          aiccg_temp(in) = 0.0d0
        end do
      end do
!
       ALUG_temp_U= 0.d0
       ALUG_temp_L= 0.d0
!
       end subroutine reset_aiccg_temp
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_temp
!
!
       deallocate(aiccg_temp)
!
       deallocate (ALUG_temp_U )
       deallocate (ALUG_temp_L )
!
       end subroutine deallocate_aiccg_temp
!
! ----------------------------------------------------------------------
!
      end module m_temp_matrix
!
