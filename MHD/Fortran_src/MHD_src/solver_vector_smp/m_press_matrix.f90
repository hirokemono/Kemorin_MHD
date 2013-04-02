!
!     module   m_press_matrix
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_aiccg_press
!       subroutine reset_aiccg_press
!       subroutine deallocate_aiccg_press
!
      module   m_press_matrix
!
      use m_precision
!
      implicit  none
!
!
!
      real(kind=kreal), allocatable, target :: aiccg_press(:)
!   coefficients of matrix
!
      integer (kind = kint) :: im_press_d
!   pointer for diagonal component
      integer (kind = kint) :: im_press_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_press_l
!   pointer for lower part of matrix
      integer (kind = kint) :: num_press_comp
!   total number of component
!
      real(kind=kreal), allocatable, target :: ALUG_press_u(:)
      real(kind=kreal), allocatable, target :: ALUG_press_l(:)
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_press
!
      use m_geometry_parameter
      use m_solver_djds_linear_fl
!
      im_press_d = 1
      im_press_l = numnod + 1
      im_press_u = numnod + itotal1_fl_l + 1
!
      num_press_comp = numnod + itotal1_fl_u + itotal1_fl_l
!
      allocate(aiccg_press(0:num_press_comp))
!
      allocate (ALUG_press_U(internal_node) )
      allocate (ALUG_press_L(internal_node) )
!
      call reset_aiccg_press
!
      end subroutine allocate_aiccg_press
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_press
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_linear_fl
!
      integer(kind = kint) :: inod, iele, in, k1
!
      aiccg_press = 0.0d0
!
      do inod = 1, numnod
        aiccg_press(inod) = 1.0d0
      end do
!
      do k1 = 1, num_t_linear
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = OLDtoNEW1(inod)
          aiccg_press(in) = 0.0d0
        end do
      end do
!
      ALUG_press_U= 1.d0
      ALUG_press_L= 1.d0
!
      end subroutine reset_aiccg_press
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_press
!
!
       deallocate(aiccg_press)
!
       deallocate (ALUG_press_U )
       deallocate (ALUG_press_L )
!
       end subroutine deallocate_aiccg_press
!
! ----------------------------------------------------------------------
!
      end module m_press_matrix
