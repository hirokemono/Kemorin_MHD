!cal_deltax_and_prods_4_nod.f90
!     module cal_deltax_and_prods_4_nod
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine cal_dx2_on_node(itype_mass)
!      subroutine cal_dxi_dxes_node(itype_mass)
!
      module cal_deltax_and_prods_4_nod
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use int_vol_elesize_on_node
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dx2_on_node(itype_mass)
!
      use m_filter_elength
!
      integer(kind = kint), intent(in) :: itype_mass
!
!
      call int_dx_ele2_node(itype_mass, elen_1%f_x2, elen_dx2_nod)
      call int_dx_ele2_node(itype_mass, elen_1%f_y2, elen_dy2_nod)
      call int_dx_ele2_node(itype_mass, elen_1%f_z2, elen_dz2_nod)
!
      call int_dx_ele2_node(itype_mass, elen_1%f_xy, elen_dxdy_nod)
      call int_dx_ele2_node(itype_mass, elen_1%f_yz, elen_dydz_nod)
      call int_dx_ele2_node(itype_mass, elen_1%f_zx, elen_dzdx_nod)
!
      end subroutine cal_dx2_on_node
!
! -----------------------------------------------------------------------
!
      subroutine cal_dxi_dxes_node(itype_mass)
!
      use m_dxi_dxes_3d_node
!
      integer(kind = kint), intent(in) :: itype_mass
!
!
      call int_dx_ele2_node(itype_mass, dxidx_ele, dxidx_nod)
      call int_dx_ele2_node(itype_mass, deidx_ele, deidx_nod)
      call int_dx_ele2_node(itype_mass, dzidx_ele, dzidx_nod)
!
      call int_dx_ele2_node(itype_mass, dxidy_ele, dxidy_nod)
      call int_dx_ele2_node(itype_mass, deidy_ele, deidy_nod)
      call int_dx_ele2_node(itype_mass, dzidy_ele, dzidy_nod)
!
      call int_dx_ele2_node(itype_mass, dxidz_ele, dxidz_nod)
      call int_dx_ele2_node(itype_mass, deidz_ele, deidz_nod)
      call int_dx_ele2_node(itype_mass, dzidz_ele, dzidz_nod)
!
      end subroutine cal_dxi_dxes_node
!
! -----------------------------------------------------------------------
!
      end module cal_deltax_and_prods_4_nod
