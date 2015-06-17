!> @file  t_filter_elength.f90
!!      module t_filter_elength
!!
!! @author  H. Matsui
!! @date Programmed in March, 2009
!!@n     Modified in Feb., 2012
!
!>@brief Strucure for element length of SGS model filtering
!!
!!@verbatim
!!      subroutine alloc_elen_ele_type(nele_filter_mom, elen_ele)
!!        integer (kind = kint), intent(in) :: nele_filter_mom
!!        type(elen_ele_diffs_type), intent(inout) :: elen_ele
!!       (substitution of allocate_ele_length )
!!      subroutine alloc_nodal_elen_type(nnod_filter_mom, elen_nod)
!!        integer (kind = kint), intent(in) :: nnod_filter_mom
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!       (substitution of allocate_nodal_ele_length )
!!
!!      subroutine alloc_ref_1d_mom_type(filter_conf)
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!       (substitution of allocate_ref_1d_moment )
!!      subroutine dealloc_filter_mom_type(FEM_elens)
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!       (substitution of deallocate_filter_moments )
!!
!!      subroutine dealloc_elen_type(elen_ele)
!!        type(elen_ele_diffs_type), intent(inout) :: elen_ele
!!       (substitution of deallocate_ele_length )
!!      subroutine dealloc_nodal_elen_type(elen_nod)
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!       (substitution of deallocate_nodal_ele_length )
!!      subroutine dealloc_ref_1d_mom_type(filter_conf)
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!       (substitution of deallocate_ref_1d_moment )
!!
!!   data comparison
!!
!!    nf_type...     nf_type
!!
!!   filter_type(:)...   filter_type
!!   f_width(:)...       f_width
!!   xmom_1d_org(:,:)... xmom_1d_org
!!          one dimensional moment in reference frame
!!              (direction,filter No,order)
!!
!!      nnod_filter_mom... filter_nod%num_fmom
!!      nele_filter_mom... filter_ele%num_fmom
!!
!!
!!      elen_dx2_nod(i)... elen_nod%moms%f_x2(i)
!!      elen_dy2_nod(i)... elen_nod%moms%f_y2(i)
!!      elen_dz2_nod(i)... elen_nod%moms%f_z2(i)
!!          ratio of element size on nodet (node ID, direction)
!!      elen_dxdy_nod(i)... elen_nod%moms%f_xy(i)
!!      elen_dydz_nod(i)... elen_nod%moms%f_yz(i)
!!      elen_dzdx_nod(i)... elen_nod%moms%f_zx(i)
!!          ratio of element size  on node (node ID, direction)
!!
!!      elen_dx2_nod_dx(i,nd)...  elen_nod%diff%df_x2(i,nd)
!!      elen_dy2_nod_dx(i,nd)...  elen_nod%diff%df_y2(i,nd)
!!      elen_dz2_nod_dx(i,nd)...  elen_nod%diff%df_z2(i,nd)
!!          1st difference of elelemet length on node
!!              (element ID, direction of diffrence)
!!      elen_dxdy_nod_dx(i,nd)... elen_nod%diff%df_xy(i,nd)
!!      elen_dydz_nod_dx(i,nd)... elen_nod%diff%df_yz(i,nd)
!!      elen_dzdx_nod_dx(i,nd)... elen_nod%diff%df_zx(i,nd)
!!          1st difference of elelemet length on node
!!              (element ID, direction of diffrence)
!!
!!      elen_dx2_ele(i)... elen_ele%moms%f_x2(i)
!!      elen_dy2_ele(i)... elen_ele%moms%f_y2(i)
!!      elen_dz2_ele(i)... elen_ele%moms%f_z2(i)
!!          ratio of element size at each element (element ID, direction)
!!      elen_dxdy_ele(i)... elen_ele%moms%f_xy(i)
!!      elen_dydz_ele(i)... elen_ele%moms%f_yz(i)
!!      elen_dzdx_ele(i)... elen_ele%moms%f_zx(i)
!!          ratio of element size at each element (element ID, direction)
!!
!!      elen_dx2_ele_dx(i,nd)...  elen_ele%diff%df_x2(i,nd)
!!      elen_dy2_ele_dx(i,nd)...  elen_ele%diff%df_y2(i,nd)
!!      elen_dz2_ele_dx(i,nd)...  elen_ele%diff%df_z2(i,nd)
!!          1st difference of elelemet length
!!              (element ID, direction of diffrence)
!!      elen_dxdy_ele_dx(i,nd)... elen_ele%diff%df_xy(i,nd)
!!      elen_dydz_ele_dx(i,nd)... elen_ele%diff%df_yz(i,nd)
!!      elen_dzdx_ele_dx(i,nd)... elen_ele%diff%df_zx(i,nd)
!!          1st difference of elelemet length
!!              (element ID, direction of diffrence)
!!
!!      elen_dx2_ele_dx2(i,nd)...  elen_ele%diff2%df_x2(i,nd)
!!      elen_dy2_ele_dx2(i,nd)...  elen_ele%diff2%df_y2(i,nd)
!!      elen_dz2_ele_dx2(i,nd)...  elen_ele%diff2%df_z2(i,nd)
!!          2nd difference of elelemet length
!!              (element ID, direction of diffrence)
!!      elen_dxdy_ele_dx2(i,nd)... elen_ele%diff2%df_xy(i,nd)
!!      elen_dydz_ele_dx2(i,nd)... elen_ele%diff2%df_yz(i,nd)
!!      elen_dzdx_ele_dx2(i,nd)... elen_ele%diff2%df_zx(i,nd)
!!          2nd difference of elelemet length
!!              (element ID, direction of diffrence)
!!
!!         i:     element ID
!!         nd:    direction of differenciate
!!@endverbatim
!
      module t_filter_elength
!
      use m_precision
!
      implicit none
!
!
!
      type filter_config_type
        integer (kind = kint) :: nf_type
        character(len=kchara), pointer :: filter_type(:)
        real(kind=kreal), pointer :: f_width(:)
        real(kind=kreal), pointer :: xmom_1d_org(:,:)
!
        character(len=kchara) :: filter_3d_head
        character(len=kchara) :: filter_line_head
        character(len=kchara) :: filter_coef_head
        character(len=kchara) :: filter_elen_head
        character(len=kchara) :: filter_moms_head
        character(len=kchara) :: filter_wide_head
      end type filter_config_type
!
      type filter_mom_type
        real(kind=kreal),   pointer :: f_0(:)
!
        real(kind=kreal),   pointer :: f_x(:)
        real(kind=kreal),   pointer :: f_y(:)
        real(kind=kreal),   pointer :: f_z(:)
        real(kind=kreal),   pointer :: f_x2(:)
        real(kind=kreal),   pointer :: f_y2(:)
        real(kind=kreal),   pointer :: f_z2(:)
        real(kind=kreal),   pointer :: f_xy(:)
        real(kind=kreal),   pointer :: f_yz(:)
        real(kind=kreal),   pointer :: f_zx(:)
      end type filter_mom_type
!
      type filter_mom_diffs_type
        real(kind=kreal),   pointer :: df_0(:,:)
!
        real(kind=kreal),   pointer :: df_x(:,:)
        real(kind=kreal),   pointer :: df_y(:,:)
        real(kind=kreal),   pointer :: df_z(:,:)
        real(kind=kreal),   pointer :: df_x2(:,:)
        real(kind=kreal),   pointer :: df_y2(:,:)
        real(kind=kreal),   pointer :: df_z2(:,:)
        real(kind=kreal),   pointer :: df_xy(:,:)
        real(kind=kreal),   pointer :: df_yz(:,:)
        real(kind=kreal),   pointer :: df_zx(:,:)
      end type filter_mom_diffs_type
!
      type elen_on_ele_type
        real(kind=kreal),   pointer :: f_x2(:)
        real(kind=kreal),   pointer :: f_y2(:)
        real(kind=kreal),   pointer :: f_z2(:)
        real(kind=kreal),   pointer :: f_xy(:)
        real(kind=kreal),   pointer :: f_yz(:)
        real(kind=kreal),   pointer :: f_zx(:)
      end type elen_on_ele_type
!
      type elen_diffs_type
        real(kind=kreal),   pointer :: df_x2(:,:)
        real(kind=kreal),   pointer :: df_y2(:,:)
        real(kind=kreal),   pointer :: df_z2(:,:)
        real(kind=kreal),   pointer :: df_xy(:,:)
        real(kind=kreal),   pointer :: df_yz(:,:)
        real(kind=kreal),   pointer :: df_zx(:,:)
      end type elen_diffs_type
!
      type nod_mom_diffs_type
        type(filter_mom_type) :: moms
        type(filter_mom_diffs_type) :: diff
      end type nod_mom_diffs_type
!
      type ele_mom_diffs_type
        type(filter_mom_type) :: moms
        type(filter_mom_diffs_type) :: diff
        type(filter_mom_diffs_type) :: diff2
      end type ele_mom_diffs_type
!
      type elen_nod_diffs_type
        type(elen_on_ele_type) :: moms
        type(elen_diffs_type) :: diff
      end type elen_nod_diffs_type
!
      type elen_ele_diffs_type
        type(elen_on_ele_type) :: moms
        type(elen_diffs_type) :: diff
        type(elen_diffs_type) :: diff2
      end type elen_ele_diffs_type
!
!
      type gradient_model_data_type
        integer (kind = kint) :: nnod_filter_mom, nele_filter_mom
        type(filter_config_type) ::  filter_conf
!
        type(elen_nod_diffs_type) :: elen_nod
        type(elen_ele_diffs_type) :: elen_ele
      end type gradient_model_data_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_moments_type(num_fmom, moms)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(filter_mom_type), intent(inout)  :: moms
!
!
      allocate( moms%f_0(num_fmom) )
!
      allocate( moms%f_x(num_fmom) )
      allocate( moms%f_y(num_fmom) )
      allocate( moms%f_z(num_fmom) )
!
      allocate( moms%f_x2(num_fmom) )
      allocate( moms%f_y2(num_fmom) )
      allocate( moms%f_z2(num_fmom) )
!
      allocate( moms%f_xy(num_fmom) )
      allocate( moms%f_yz(num_fmom) )
      allocate( moms%f_zx(num_fmom) )
!
      if (num_fmom .gt. 0) then
        moms%f_0 =  0.0d0
        moms%f_x =  0.0d0
        moms%f_y =  0.0d0
        moms%f_z =  0.0d0
        moms%f_x2 = 0.0d0
        moms%f_y2 = 0.0d0
        moms%f_z2 = 0.0d0
        moms%f_xy = 0.0d0
        moms%f_yz = 0.0d0
        moms%f_zx = 0.0d0
      end if 
!
      end subroutine alloc_moments_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_filter_mom_diffs_type(num_fmom, dmoms)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(filter_mom_diffs_type), intent(inout)  :: dmoms
!
!
      allocate( dmoms%df_0(num_fmom,3) )
!
      allocate( dmoms%df_x(num_fmom,3) )
      allocate( dmoms%df_y(num_fmom,3) )
      allocate( dmoms%df_z(num_fmom,3) )
!
      allocate( dmoms%df_x2(num_fmom,3) )
      allocate( dmoms%df_y2(num_fmom,3) )
      allocate( dmoms%df_z2(num_fmom,3) )
!
      allocate( dmoms%df_xy(num_fmom,3) )
      allocate( dmoms%df_yz(num_fmom,3) )
      allocate( dmoms%df_zx(num_fmom,3) )
!
      if (num_fmom .gt. 0) then
        dmoms%df_0 =  0.0d0
        dmoms%df_x =  0.0d0
        dmoms%df_y =  0.0d0
        dmoms%df_z =  0.0d0
        dmoms%df_x2 = 0.0d0
        dmoms%df_y2 = 0.0d0
        dmoms%df_z2 = 0.0d0
        dmoms%df_xy = 0.0d0
        dmoms%df_yz = 0.0d0
        dmoms%df_zx = 0.0d0
      end if 
!
      end subroutine alloc_filter_mom_diffs_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_elen_on_ele_type(num, elens)
!
      integer (kind = kint), intent(in) :: num
      type(elen_on_ele_type), intent(inout)  :: elens
!
!
      allocate( elens%f_x2(num) )
      allocate( elens%f_y2(num) )
      allocate( elens%f_z2(num) )
!
      allocate( elens%f_xy(num) )
      allocate( elens%f_yz(num) )
      allocate( elens%f_zx(num) )
!
      call clear_elen_on_ele_type(num, elens)
!
      end subroutine alloc_elen_on_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_elen_diffs_type(num_elen, elen_d)
!
      integer (kind = kint), intent(in) :: num_elen
      type(elen_diffs_type), intent(inout)  :: elen_d
!
!
      allocate( elen_d%df_x2(num_elen,3) )
      allocate( elen_d%df_y2(num_elen,3) )
      allocate( elen_d%df_z2(num_elen,3) )
!
      allocate( elen_d%df_xy(num_elen,3) )
      allocate( elen_d%df_yz(num_elen,3) )
      allocate( elen_d%df_zx(num_elen,3) )
!
      call clear_elen_diffs_type(num_elen, elen_d)
!
      end subroutine alloc_elen_diffs_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine clear_elen_on_ele_type(num, elens)
!
      integer (kind = kint), intent(in) :: num
      type(elen_on_ele_type), intent(inout)  :: elens
!
!
      if (num .gt. 0) then
!$omp workshare
        elens%f_x2 = 0.0d0
        elens%f_y2 = 0.0d0
        elens%f_z2 = 0.0d0
        elens%f_xy = 0.0d0
        elens%f_yz = 0.0d0
        elens%f_zx = 0.0d0
!$omp end workshare
      end if 
!
      end subroutine clear_elen_on_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine clear_elen_diffs_type(num, elen_d)
!
      integer (kind = kint), intent(in) :: num
      type(elen_diffs_type), intent(inout)  :: elen_d
!
!
      if (num .gt. 0) then
!$omp workshare
        elen_d%df_x2 = 0.0d0
        elen_d%df_y2 = 0.0d0
        elen_d%df_z2 = 0.0d0
        elen_d%df_xy = 0.0d0
        elen_d%df_yz = 0.0d0
        elen_d%df_zx = 0.0d0
!$omp end workshare
      end if 
!
      end subroutine clear_elen_diffs_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_moments_type(moms)
!
      type(filter_mom_type), intent(inout)  :: moms
!
!
      deallocate( moms%f_0 )
      deallocate( moms%f_x, moms%f_y, moms%f_z )
      deallocate( moms%f_x2, moms%f_y2, moms%f_z2 )
      deallocate( moms%f_xy, moms%f_yz, moms%f_zx )
!
      end subroutine dealloc_moments_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_mom_diffs_type(dmoms)
!
      type(filter_mom_diffs_type), intent(inout)  :: dmoms
!
!
      deallocate( dmoms%df_0 )
      deallocate( dmoms%df_x, dmoms%df_y, dmoms%df_z )
      deallocate( dmoms%df_x2, dmoms%df_y2, dmoms%df_z2 )
      deallocate( dmoms%df_xy, dmoms%df_yz, dmoms%df_zx )
!
      end subroutine dealloc_filter_mom_diffs_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_elen_on_ele_type(elens)
!
      type(elen_on_ele_type), intent(inout)  :: elens
!
!
      deallocate( elens%f_x2, elens%f_y2, elens%f_z2 )
      deallocate( elens%f_xy, elens%f_yz, elens%f_zx )
!
      end subroutine dealloc_elen_on_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_elen_diffs_type(elen_d)
!
      type(elen_diffs_type), intent(inout)  :: elen_d
!
!
      deallocate( elen_d%df_x2, elen_d%df_y2, elen_d%df_z2 )
      deallocate( elen_d%df_xy, elen_d%df_yz, elen_d%df_zx )
!
      end subroutine dealloc_elen_diffs_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_nod_mom_diffs_type(moms_nod)
!
      type(nod_mom_diffs_type), intent(inout)  :: moms_nod
!
!
      call dealloc_moments_type(moms_nod%moms)
      call dealloc_filter_mom_diffs_type(moms_nod%diff)
!
      end subroutine dealloc_nod_mom_diffs_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ele_mom_diffs_type(moms_ele)
!
      type(ele_mom_diffs_type), intent(inout)  :: moms_ele
!
!
      call dealloc_moments_type(moms_ele%moms)
      call dealloc_filter_mom_diffs_type(moms_ele%diff)
      call dealloc_filter_mom_diffs_type(moms_ele%diff2)
!
      end subroutine dealloc_ele_mom_diffs_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_elen_ele_type(nele_filter_mom, elen_ele)
!
      integer (kind = kint), intent(in) :: nele_filter_mom
      type(elen_ele_diffs_type), intent(inout) :: elen_ele
!
!
      call alloc_elen_on_ele_type(nele_filter_mom, elen_ele%moms)
      call alloc_elen_diffs_type(nele_filter_mom,  elen_ele%diff)
      call alloc_elen_diffs_type(nele_filter_mom,  elen_ele%diff2)
!
      end subroutine alloc_elen_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_nodal_elen_type(nnod_filter_mom, elen_nod)
!
      integer (kind = kint), intent(in) :: nnod_filter_mom
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
!
      call alloc_elen_on_ele_type(nnod_filter_mom, elen_nod%moms)
      call alloc_elen_diffs_type(nnod_filter_mom, elen_nod%diff)
!
      end subroutine alloc_nodal_elen_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_ref_1d_mom_type(filter_conf)
!
      type(filter_config_type), intent(inout) ::  filter_conf
!
!
      allocate( filter_conf%filter_type(filter_conf%nf_type) )
      allocate( filter_conf%f_width(filter_conf%nf_type) )
!
      allocate( filter_conf%xmom_1d_org(filter_conf%nf_type,0:2) )
!
      filter_conf%f_width =      0.0d0
      filter_conf%xmom_1d_org =  0.0d0
!
      end subroutine alloc_ref_1d_mom_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_mom_type(FEM_elens)
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call dealloc_elen_type(FEM_elens%elen_ele)
      call dealloc_nodal_elen_type(FEM_elens%elen_nod)
      call dealloc_ref_1d_mom_type(FEM_elens%filter_conf)
!
      end subroutine dealloc_filter_mom_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_elen_type(elen_ele)
!
      type(elen_ele_diffs_type), intent(inout) :: elen_ele
!
!
      call dealloc_elen_on_ele_type(elen_ele%moms)
      call dealloc_elen_diffs_type(elen_ele%diff)
      call dealloc_elen_diffs_type(elen_ele%diff2)
!
      end subroutine dealloc_elen_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_nodal_elen_type(elen_nod)
!
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
!
      call dealloc_elen_on_ele_type(elen_nod%moms)
      call dealloc_elen_diffs_type(elen_nod%diff)
!
      end subroutine dealloc_nodal_elen_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ref_1d_mom_type(filter_conf)
!
      type(filter_config_type), intent(inout) ::  filter_conf
!
!
      deallocate( filter_conf%filter_type, filter_conf%f_width )
      deallocate( filter_conf%xmom_1d_org )
!
      end subroutine dealloc_ref_1d_mom_type
!
!  ---------------------------------------------------------------------
!
      end module t_filter_elength
