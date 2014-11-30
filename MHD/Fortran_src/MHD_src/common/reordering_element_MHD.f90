!reordering_element_MHD.f90
!     module reordering_element_MHD
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!      Moified by H. Matsui on Feb., 2008
!
!      subroutine reordering_element_info
!   ordereing of connectivity, element group, and surface group
!
!      subroutine reordering_element_connect(numele, nnod_4_ele,        &
!     &          new2oldele_layer, iele_gl_org, iele_gl, ie_org, ie)
!      subroutine reordering_element_group(numele, old2newele_layer,    &
!     &    num_mat_bc, mat_item)
!      subroutine reordering_surface_group(numele, old2newele_layer,    &
!     &    num_surf_bc, surf_item)
!
      module reordering_element_MHD
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reordering_element_info
!
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_element_group
      use m_surface_group
      use m_work_4_MHD_layering
!
!
      call reordering_element_connect(numele, nnod_4_ele,               &
     &    new2oldele_layer(1), iele_global_org(1), iele_global(1),      &
     &    ie_org(1,1), ie(1,1) )
!
      call reordering_element_group(numele, old2newele_layer(1),        &
     &    num_mat_bc, mat_item(1) )
!
      call reordering_surface_group(numele, old2newele_layer(1),        &
     &    num_surf_bc, surf_item(1,1) )
!
      end subroutine reordering_element_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine reordering_element_connect(numele, nnod_4_ele,         &
     &          new2oldele_layer, iele_gl_org, iele_gl, ie_org, ie)
!
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: new2oldele_layer(numele)
!
      integer (kind = kint_gl), intent(inout) :: iele_gl_org(numele)
      integer (kind = kint_gl), intent(inout) :: iele_gl(numele)
      integer (kind = kint), intent(inout) :: ie_org(numele,nnod_4_ele)
      integer (kind = kint), intent(inout) :: ie(numele,nnod_4_ele)
!
      integer (kind = kint) :: k, iele, iele0
!
!
      iele_gl_org(1:numele) = iele_gl(1:numele)
      do k = 1, nnod_4_ele
        ie_org(1:numele,k) = ie(1:numele,k)
      end do
!
      do iele = 1, numele
        iele0 = new2oldele_layer(iele)
        iele_gl(iele) = iele_gl_org(iele0)
        do k = 1, nnod_4_ele
          ie(iele,k) = ie_org(iele0,k)
        end do
      end do
!
      end subroutine reordering_element_connect
!
! -----------------------------------------------------------------------
!
      subroutine reordering_element_group(numele, old2newele_layer,     &
     &          num_mat_bc, mat_item)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: old2newele_layer(numele)
      integer (kind = kint), intent(in) :: num_mat_bc
!
      integer (kind = kint), intent(inout) :: mat_item(num_mat_bc)
!
      integer (kind = kint) :: iele, iele0, inum
!
!
      do inum = 1, num_mat_bc
        iele0 = mat_item(inum)
        iele = old2newele_layer(iele0)
        mat_item(inum) = iele
      end do
!
      end subroutine reordering_element_group
!
! -----------------------------------------------------------------------
!
      subroutine reordering_surface_group(numele, old2newele_layer,     &
     &          num_surf_bc, surf_item)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: old2newele_layer(numele)
      integer (kind = kint), intent(in) :: num_surf_bc
!
      integer (kind = kint), intent(inout) :: surf_item(2,num_surf_bc)
!
      integer (kind = kint) :: iele, iele0, inum
!
!
      do inum = 1, num_surf_bc
        iele0 = surf_item(1,inum)
        iele = old2newele_layer(iele0)
        surf_item(1,inum) = iele
      end do
!
      end subroutine reordering_surface_group
!
!  ---------------------------------------------------------------------
!
      end module reordering_element_MHD
