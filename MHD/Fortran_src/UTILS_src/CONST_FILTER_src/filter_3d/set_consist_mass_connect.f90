!
!     module set_consist_mass_connect
!
!     Written by H. Matsui on Oct., 2006
!
!      subroutine s_set_consist_mass_connect(numnod)
!
      module set_consist_mass_connect
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_consist_mass_connect(numnod)
!
      use m_crs_matrix
      use m_crs_consist_mass_mat
!
      integer(kind = kint), intent(in) :: numnod
!
!
      ntot_mass_l = tbl1_crs%ntot_l
      ntot_mass_u = tbl1_crs%ntot_u
!
      im_mass_d = 1
      im_mass_l = numnod + 1
      im_mass_u = numnod + tbl1_crs%ntot_l + 1
      num_mass_mat = numnod + tbl1_crs%ntot_l + tbl1_crs%ntot_u
!
!
      call allocate_mass_connect(numnod)
!
      istack_mass_l(0:numnod) = tbl1_crs%istack_l
      istack_mass_u(0:numnod) = tbl1_crs%istack_u
!
      item_mass_l(1:ntot_mass_l) = tbl1_crs%item_l
      item_mass_u(1:ntot_mass_u) = tbl1_crs%item_u
!
      call allocate_aiccg_mass
!
      end subroutine s_set_consist_mass_connect
!
!-----------------------------------------------------------------------
!
      end module set_consist_mass_connect
