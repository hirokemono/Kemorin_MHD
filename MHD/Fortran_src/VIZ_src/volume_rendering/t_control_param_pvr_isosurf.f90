!>@file   t_control_param_pvr_isosurf.f90
!!@brief  module t_control_param_pvr_isosurf
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set each PVR parameters from control
!!
!!@verbatim
!!      subroutine set_control_pvr_isosurfs(num_nod_phys, phys_nod_name,&
!!     &                                    pvr_isos_c, pvr_isos_p)
!!      subroutine alloc_pvr_isosurfs_data(field_pvr, pvr_iso_p)
!!      subroutine set_pvr_isosurfs_data(node, ele, g_FEM, jac_3d,      &
!!     &                                 nod_fld, pvr_isos_p)
!!
!!      subroutine dealloc_pvr_isosurf_param(pvr_isos_p)
!!      subroutine dealloc_pvr_isosurfs_data(pvr_isos_p)
!!        type(pvr_isosurfs_ctl), intent(in) :: pvr_isos_c
!!        type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!!@endverbatim
!
      module t_control_param_pvr_isosurf
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use calypso_mpi
!
      use t_control_param_4_pvr_field
      use t_pvr_field_data
!
      implicit  none
!
!>  Structure of parameters for isosurface in PVR
      type pvr_isosurf_parameter
!>        Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>        Number of isosurfaces
        integer(kind = kint) :: itype_isosurf
!>        field value for isosurfaces
        real(kind = kreal) :: iso_value
!>        Opacity value for isosurfaces
        real(kind = kreal) :: iso_opacity
!
!>        field paramters for isosurface
        type(pvr_field_parameter) :: iso_fld_param
!>        field paramters for isosurface
        integer(kind = kint) :: icheck_iso_ncomp(1)
!
!>      Structure for field data for PVR
        type(pvr_field_data), pointer :: field_iso
      end type pvr_isosurf_parameter
!
!>  Structure of parameters for isosurfaces in PVR
      type pvr_isosurfs_parameter
!>        Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>        Structure of parameters for isosurface in PVR
        type(pvr_isosurf_parameter), allocatable :: pvr_iso_p(:)
      end type pvr_isosurfs_parameter
!
      private :: alloc_pvr_isosurfs_param, set_control_pvr_isosurf
      private :: alloc_pvr_isosurf_data, dealloc_pvr_isosurf_data
      private :: cal_field_4_each_pvr_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_isosurfs(num_nod_phys, phys_nod_name,  &
     &                                    pvr_isos_c, pvr_isos_p)
!
      use t_control_data_pvr_isosurfs
      use t_control_array_character
      use pvr_surface_enhancement
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(pvr_isosurfs_ctl), intent(in) :: pvr_isos_c
!
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
!
      integer(kind = kint) ::  i
!
!
      pvr_isos_p%num_isosurf = pvr_isos_c%num_pvr_iso_ctl
      call alloc_pvr_isosurfs_param(pvr_isos_p)
      if(pvr_isos_p%num_isosurf .le. 0) return
!
      do i = 1, pvr_isos_p%num_isosurf
        call set_control_pvr_isosurf(num_nod_phys, phys_nod_name,       &
     &      pvr_isos_c%pvr_iso_ctl(i), pvr_isos_p%pvr_iso_p(i))
      end do
!
      end subroutine set_control_pvr_isosurfs
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfs_data(field_pvr, pvr_isos_p)
!
      type(pvr_field_data), intent(in), target :: field_pvr
!
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
!
      integer(kind = kint) ::  i
!
      do i = 1, pvr_isos_p%num_isosurf
        call alloc_pvr_isosurf_data(field_pvr, pvr_isos_p%pvr_iso_p(i))
      end do
!
      end subroutine alloc_pvr_isosurfs_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_isosurfs_data(node, ele, g_FEM, jac_3d,        &
     &                                 nod_fld, pvr_isos_p)
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_geometries_in_pvr_screen
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data), intent(in) :: nod_fld
!
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
!
      integer(kind = kint) ::  i
!
      do i = 1, pvr_isos_p%num_isosurf
        call cal_field_4_each_pvr_iso(node, ele, g_FEM, jac_3d,         &
     &                                nod_fld, pvr_isos_p%pvr_iso_p(i))
      end do
!
      end subroutine set_pvr_isosurfs_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurf_param(pvr_isos_p)
!
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
!
!
      deallocate(pvr_isos_p%pvr_iso_p)
!
      end subroutine dealloc_pvr_isosurf_param
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfs_data(pvr_isos_p)
!
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
!
      integer(kind = kint) ::  i
!
      do i = 1, pvr_isos_p%num_isosurf
        call dealloc_pvr_isosurf_data(pvr_isos_p%pvr_iso_p(i))
      end do
!
      end subroutine dealloc_pvr_isosurfs_data
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfs_param(pvr_isos_p)
!
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
!
!
      allocate(pvr_isos_p%pvr_iso_p(pvr_isos_p%num_isosurf))
!
      end subroutine alloc_pvr_isosurfs_param
!
! -----------------------------------------------------------------------
!
      subroutine set_control_pvr_isosurf(num_nod_phys, phys_nod_name,   &
     &                                   pvr_iso_ctl, pvr_iso_p)
!
      use m_more_component_flags
      use t_ctl_data_pvr_isosurface
      use t_control_array_character
      use pvr_surface_enhancement
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(pvr_isosurf_ctl), intent(in) :: pvr_iso_ctl
!
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
      character(len = kchara) :: tmpchara
!
!
      if(pvr_iso_ctl%isosurf_data_ctl%iflag .gt. 0                      &
     &    .and. pvr_iso_ctl%isosurf_comp_ctl%iflag .gt. 0) then
        call set_control_field_4_pvr                                    &
     &     (pvr_iso_ctl%isosurf_data_ctl, pvr_iso_ctl%isosurf_comp_ctl, &
     &      num_nod_phys, phys_nod_name,                                &
     &      pvr_iso_p%iso_fld_param, pvr_iso_p%icheck_iso_ncomp)
      else
        pvr_iso_p%iso_fld_param%id_field =          -1
        pvr_iso_p%iso_fld_param%id_component =      -1
        pvr_iso_p%iso_fld_param%num_original_comp =  1
        pvr_iso_p%iso_fld_param%field_name =        ''
        pvr_iso_p%icheck_iso_ncomp = -1
      end if
!
      if(pvr_iso_ctl%iso_value_ctl%iflag .gt. 0) then
        pvr_iso_p%iso_value = pvr_iso_ctl%iso_value_ctl%realvalue
      end if
!
      if(pvr_iso_ctl%opacity_ctl%iflag .gt. 0) then
        pvr_iso_p%iso_opacity = pvr_iso_ctl%opacity_ctl%realvalue
      end if
!
      if(pvr_iso_ctl%isosurf_type_ctl%iflag .gt. 0) then
        tmpchara = pvr_iso_ctl%isosurf_type_ctl%charavalue
        if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
          pvr_iso_p%itype_isosurf = IFLAG_SHOW_REVERSE
        else if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
          pvr_iso_p%itype_isosurf = IFLAG_SHOW_FORWARD
        else
          pvr_iso_p%itype_isosurf = IFLAG_SHOW_FORWARD
        end if
      end if
!
      end subroutine set_control_pvr_isosurf
!
!  ---------------------------------------------------------------------
!
      subroutine cal_field_4_each_pvr_iso(node, ele, g_FEM, jac_3d,     &
     &                                    nod_fld, pvr_iso_p)
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_geometries_in_pvr_screen
      use t_control_param_4_pvr_field
      use cal_gradient_on_element
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data), intent(in) :: nod_fld
!
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
!
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      if(pvr_iso_p%iso_fld_param%id_field .eq. -1) return
      i_field = pvr_iso_p%iso_fld_param%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    pvr_iso_p%iso_fld_param%id_component,                         &
     &    nod_fld%d_fld(1,ist_fld+1), pvr_iso_p%field_iso%d_pvr)
      call fem_gradient_on_element(ele%istack_ele_smp, node%numnod,     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, ione, jac_3d%dnx, jac_3d%xjac,  &
     &    pvr_iso_p%field_iso%grad_ele, pvr_iso_p%field_iso%d_pvr)
!
      end subroutine cal_field_4_each_pvr_iso
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurf_data(field_pvr, pvr_iso_p)
!
      type(pvr_field_data), intent(in), target :: field_pvr
!
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
!
      if(pvr_iso_p%iso_fld_param%id_field .eq. -1) then
        pvr_iso_p%field_iso => field_pvr
      else
        allocate(pvr_iso_p%field_iso)
        call alloc_nod_data_4_pvr(field_pvr%numnod, field_pvr%numele,   &
     &                            pvr_iso_p%field_iso)
      end if
!
      end subroutine alloc_pvr_isosurf_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurf_data(pvr_iso_p)
!
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
!
      if(pvr_iso_p%iso_fld_param%id_field .eq. -1) then
        nullify(pvr_iso_p%field_iso)
      else
        call dealloc_nod_data_4_pvr(pvr_iso_p%field_iso)
        deallocate(pvr_iso_p%field_iso)
      end if
!
      end subroutine dealloc_pvr_isosurf_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_pvr_isosurf
