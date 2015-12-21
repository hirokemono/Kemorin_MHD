!
!     module average_on_elements
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!
!      subroutine velocity_on_element
!      subroutine magnetic_on_element
!      subroutine filtered_magne_on_ele
!
!      subroutine vorticity_on_element
!      subroutine rot_magne_on_element
!      subroutine current_on_element
!      subroutine rot_filter_magne_on_element
!
      module average_on_elements
!
      use m_precision
      use m_machine_parameter
!
      use m_control_parameter
      use m_geometry_data
      use m_int_vol_data
!
      use cal_fields_on_element
      use cal_differences_on_ele
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine velocity_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
!
!
      call vector_on_element_1st(iele_fl_smp_stack, intg_point_t_evo,   &
     &    nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld,             &
     &    fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%iflag_update,  &
     &    fld_ele1%d_fld)
!
      end subroutine velocity_on_element
!
! -----------------------------------------------------------------------
!
      subroutine magnetic_on_element
!
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
!
!
      call vector_on_element_1st(ele1%istack_ele_smp, intg_point_t_evo, &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld,            &
     &    fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%iflag_update, &
     &    fld_ele1%d_fld)
!
      end subroutine magnetic_on_element
!
! -----------------------------------------------------------------------
!
      subroutine filtered_magne_on_ele
!
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
!
!
      call vector_on_element_1st(ele1%istack_ele_smp, intg_point_t_evo, &
     &    nod_fld1%ntot_phys, iphys%i_filter_magne, nod_fld1%d_fld,     &
     &    fld_ele1%ntot_phys, iphys_ele%i_filter_magne,                 &
     &    fld_ele1%iflag_update, fld_ele1%d_fld)
!
      end subroutine filtered_magne_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine vorticity_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
!
!
      call rotation_on_element_1st(iele_fl_smp_stack, intg_point_t_evo, &
     &    nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld,             &
     &    fld_ele1%ntot_phys, iphys_ele%i_vort,                         &
     &    fld_ele1%iflag_update, fld_ele1%d_fld)
!
      end subroutine vorticity_on_element
!
! -----------------------------------------------------------------------
!
      subroutine rot_magne_on_element
!
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
!
!
      call rotation_on_element_1st                                      &
     &   (ele1%istack_ele_smp, intg_point_t_evo,                        &
     &    nod_fld1%ntot_phys, iphys%i_vecp, nod_fld1%d_fld,             &
     &    fld_ele1%ntot_phys, iphys_ele%i_magne,                        &
     &    fld_ele1%iflag_update, fld_ele1%d_fld)
!
      end subroutine rot_magne_on_element
!
! -----------------------------------------------------------------------
!
      subroutine current_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
!
!
      call rotation_on_element_1st(iele_cd_smp_stack, intg_point_t_evo, &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld,            &
     &    fld_ele1%ntot_phys, iphys_ele%i_current,                      &
     &    fld_ele1%iflag_update, fld_ele1%d_fld)
!
      end subroutine current_on_element
!
! -----------------------------------------------------------------------
!
      subroutine rot_filter_magne_on_element
!
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
!
!
      call rotation_on_element_1st                                      &
     &   (ele1%istack_ele_smp, intg_point_t_evo,                        &
     &    nod_fld1%ntot_phys, iphys%i_filter_vecp, nod_fld1%d_fld,      &
     &    fld_ele1%ntot_phys, iphys_ele%i_filter_magne,                 &
     &    fld_ele1%iflag_update, fld_ele1%d_fld)
!
      end subroutine rot_filter_magne_on_element
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine vector_on_element_1st                                  &
     &         (iele_fsmp_stack, n_int, ncomp_nod, ifld_nod, d_nod,     &
     &          ncomp_ele, ifld_ele, iflag_update, d_ele)
!
      use m_jacobians
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_nod, ifld_nod
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,ncomp_nod)
!
      integer(kind = kint), intent(in) :: ncomp_ele, ifld_ele
      integer(kind = kint), intent(inout) :: iflag_update(ncomp_ele)
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,ncomp_ele)
!
!
      call vector_on_element(node1, ele1, jac1_3d_q, iele_fsmp_stack,   &
     &    n_int, d_nod(1,ifld_nod), d_ele(1,ifld_ele))
      iflag_update(ifld_ele:ifld_ele+2) = 1
!
      end subroutine vector_on_element_1st
!
! -----------------------------------------------------------------------
!
      subroutine rotation_on_element_1st                                &
     &         (iele_fsmp_stack, n_int, ncomp_nod, ifld_nod, d_nod,     &
     &          ncomp_ele, ifld_ele, iflag_update, d_ele)
!
      use m_jacobians
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_nod, ifld_nod
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,ncomp_nod)
!
      integer(kind = kint), intent(in) :: ncomp_ele, ifld_ele
      integer(kind = kint), intent(inout) :: iflag_update(ncomp_ele)
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,ncomp_ele)
!
!
      call rotation_on_element(node1, ele1, jac1_3d_q,                  &
     &    iele_fsmp_stack, n_int, d_nod(1,ifld_nod), d_ele(1,ifld_ele))
      iflag_update(ifld_ele:ifld_ele+2) = 1
!
      end subroutine rotation_on_element_1st
!
! -----------------------------------------------------------------------
!
      end module average_on_elements
