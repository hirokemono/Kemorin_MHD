!cal_deltax_and_prods_4_nod.f90
!     module cal_deltax_and_prods_4_nod
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine cal_dx2_on_node(itype_mass)
!      subroutine cal_dxi_dxes_node(itype_mass, dxidxs)
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
      call int_dx_ele2_node(itype_mass, FEM1_elen%elen_ele%moms%f_x2,   &
     &   FEM1_elen%elen_nod%moms%f_x2)
      call int_dx_ele2_node(itype_mass, FEM1_elen%elen_ele%moms%f_y2,   &
     &   FEM1_elen%elen_nod%moms%f_y2)
      call int_dx_ele2_node(itype_mass, FEM1_elen%elen_ele%moms%f_z2,   &
     &   FEM1_elen%elen_nod%moms%f_z2)
!
      call int_dx_ele2_node(itype_mass, FEM1_elen%elen_ele%moms%f_xy,   &
     &   FEM1_elen%elen_nod%moms%f_xy)
      call int_dx_ele2_node(itype_mass, FEM1_elen%elen_ele%moms%f_yz,   &
     &   FEM1_elen%elen_nod%moms%f_yz)
      call int_dx_ele2_node(itype_mass, FEM1_elen%elen_ele%moms%f_zx,   &
     &   FEM1_elen%elen_nod%moms%f_zx)
!
      end subroutine cal_dx2_on_node
!
! -----------------------------------------------------------------------
!
      subroutine cal_dxi_dxes_node(itype_mass, dxidxs)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: itype_mass
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_ele%dxi%df_dx, dxidxs%dx_nod%dxi%df_dx)
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_nod%dxi%df_dy, dxidxs%dx_nod%dxi%df_dy)
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_nod%dxi%df_dz, dxidxs%dx_nod%dxi%df_dz)
!
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_ele%dei%df_dx, dxidxs%dx_nod%dei%df_dx)
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_nod%dei%df_dy, dxidxs%dx_nod%dei%df_dy)
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_nod%dei%df_dz, dxidxs%dx_nod%dei%df_dz)
!
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_ele%dzi%df_dx, dxidxs%dx_nod%dzi%df_dx)
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_nod%dzi%df_dy, dxidxs%dx_nod%dzi%df_dy)
      call int_dx_ele2_node                                             &
     &   (itype_mass, dxidxs%dx_nod%dzi%df_dz, dxidxs%dx_nod%dzi%df_dz)
!
      end subroutine cal_dxi_dxes_node
!
! -----------------------------------------------------------------------
!
      end module cal_deltax_and_prods_4_nod
