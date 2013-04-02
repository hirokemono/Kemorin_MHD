!
!     module   m_light_element_matrix
!.......................................................................
!
!       subroutine allocate_aiccg_composit
!       subroutine reset_aiccg_composit
!       subroutine deallocate_aiccg_composit
!
      module   m_light_element_matrix
!
      use m_precision
!
      implicit  none
!
!
      real (kind=kreal), allocatable, target :: aiccg_composit(:)
!   coefficients of matrix
!
      integer (kind = kint) :: im_cps_d
!   pointer for diagonal component
      integer (kind = kint) :: im_cps_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_cps_l
!   pointer for lower part of matrix
      integer (kind = kint) :: num_composit_comp
!   total number of component
!
      real(kind=kreal), allocatable, target :: ALUG_composit_u(:)
      real(kind=kreal), allocatable, target :: ALUG_composit_l(:)
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
       im_cps_d = 1
       im_cps_l = numnod + 1
       im_cps_u = numnod + itotal_fl_l + 1
!
       num_composit_comp = numnod + itotal_fl_l + itotal_fl_u
!
       allocate(aiccg_composit(0:num_composit_comp))
!
       allocate (ALUG_composit_u(internal_node) )
       allocate (ALUG_composit_l(internal_node) )
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
      aiccg_composit = 0.0d0
!
      do inod = 1, numnod
        aiccg_composit(inod) = 1.0d0
      end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = OLDtoNEW(inod)
          aiccg_composit(in) = 0.0d0
        end do
      end do
!
       ALUG_composit_u= 0.d0
       ALUG_composit_l= 0.d0
!
       end subroutine reset_aiccg_composit
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_composit
!
!
       deallocate(aiccg_composit)
!
       deallocate (ALUG_composit_u)
       deallocate (ALUG_composit_l)
!
       end subroutine deallocate_aiccg_composit
!
! ----------------------------------------------------------------------
!
      end module m_light_element_matrix
