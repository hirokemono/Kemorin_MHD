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
!!      subroutine alloc_nodal_elen_type(nnod_filter_mom, elen_nod)
!!        integer (kind = kint), intent(in) :: nnod_filter_mom
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!
!!      subroutine alloc_ref_1d_mom_type(filter_conf)
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!      subroutine dealloc_filter_mom_type(FEM_elens)
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!
!!      subroutine dealloc_elen_type(elen_ele)
!!        type(elen_ele_diffs_type), intent(inout) :: elen_ele
!!      subroutine dealloc_nodal_elen_type(elen_nod)
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!      subroutine dealloc_ref_1d_mom_type(filter_conf)
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!
!!      subroutine copy_filter_elen_ele_from_type(elen_e, FEM_elens)
!!         type(elen_ele_diffs_type), intent(in)  :: elen_e
!!         type(gradient_model_data_type), intent(inout) :: FEM_elens
!!      subroutine copy_filter_elen_ele_to_type(FEM_elens, elen_e)
!!         type(gradient_model_data_type), intent(in) :: FEM_elens
!!         type(elen_ele_diffs_type), intent(inout) :: elen_e
!!      subroutine copy_elength_type(num, elen_org, elen_tgt)
!!        integer (kind = kint), intent(in) :: num
!!        type(elen_on_ele_type), intent(in) :: elen_org
!!        type(elen_on_ele_type), intent(inout) :: elen_tgt
!!      subroutine copy_elen_diffs_type(num, elen_org, elen_tgt)
!!        integer (kind = kint), intent(in) :: num
!!        type(elen_diffs_type), intent(in) :: elen_org
!!        type(elen_diffs_type), intent(inout) :: elen_tgt
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
        character(len=kchara), allocatable :: filter_type(:)
        real(kind=kreal), allocatable :: f_width(:)
        real(kind=kreal), allocatable :: xmom_1d_org(:,:)
!
        character(len=kchara) :: filter_3d_head
        character(len=kchara) :: filter_line_head
        character(len=kchara) :: filter_coef_head
        character(len=kchara) :: filter_elen_head
        character(len=kchara) :: filter_moms_head
        character(len=kchara) :: filter_wide_head
      end type filter_config_type
!
      type elen_on_ele_type
        real(kind=kreal),   allocatable :: f_x2(:)
        real(kind=kreal),   allocatable :: f_y2(:)
        real(kind=kreal),   allocatable :: f_z2(:)
        real(kind=kreal),   allocatable :: f_xy(:)
        real(kind=kreal),   allocatable :: f_yz(:)
        real(kind=kreal),   allocatable :: f_zx(:)
      end type elen_on_ele_type
!
      type elen_diffs_type
        real(kind=kreal),   allocatable :: df_x2(:,:)
        real(kind=kreal),   allocatable :: df_y2(:,:)
        real(kind=kreal),   allocatable :: df_z2(:,:)
        real(kind=kreal),   allocatable :: df_xy(:,:)
        real(kind=kreal),   allocatable :: df_yz(:,:)
        real(kind=kreal),   allocatable :: df_zx(:,:)
      end type elen_diffs_type
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
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_from_type(elen_e, FEM_elens)
!
      type(elen_ele_diffs_type), intent(in)  :: elen_e
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call copy_elength_type(FEM_elens%nele_filter_mom,                 &
     &    elen_e%moms,  FEM_elens%elen_ele%moms)
      call copy_elen_diffs_type(FEM_elens%nele_filter_mom,              &
     &    elen_e%diff, FEM_elens%elen_ele%diff)
      call copy_elen_diffs_type(FEM_elens%nele_filter_mom,              &
     &    elen_e%diff2, FEM_elens%elen_ele%diff2)
!
      end subroutine copy_filter_elen_ele_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_to_type(FEM_elens, elen_e)
!
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(elen_ele_diffs_type), intent(inout) :: elen_e
!
!
      call copy_elength_type(FEM_elens%nele_filter_mom,                 &
     &    FEM_elens%elen_ele%moms, elen_e%moms)
      call copy_elen_diffs_type(FEM_elens%nele_filter_mom,              &
     &    FEM_elens%elen_ele%diff, elen_e%diff)
      call copy_elen_diffs_type(FEM_elens%nele_filter_mom,              &
     &    FEM_elens%elen_ele%diff2, elen_e%diff2)
!
      end subroutine copy_filter_elen_ele_to_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_elength_type(num, elen_org, elen_tgt)
!
      integer (kind = kint), intent(in) :: num
      type(elen_on_ele_type), intent(in) :: elen_org
      type(elen_on_ele_type), intent(inout) :: elen_tgt
!
      integer (kind=kint) :: i
!
!
!$omp parallel do
      do i = 1, num
        elen_tgt%f_x2(i) = elen_org%f_x2(i)
        elen_tgt%f_y2(i) = elen_org%f_y2(i)
        elen_tgt%f_z2(i) = elen_org%f_z2(i)
        elen_tgt%f_xy(i) = elen_org%f_xy(i)
        elen_tgt%f_yz(i) = elen_org%f_yz(i)
        elen_tgt%f_zx(i) = elen_org%f_zx(i)
      end do
!$omp end parallel do
!
      end subroutine copy_elength_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_elen_diffs_type(num, elen_org, elen_tgt)
!
      integer (kind = kint), intent(in) :: num
      type(elen_diffs_type), intent(in) :: elen_org
      type(elen_diffs_type), intent(inout) :: elen_tgt
!
      integer (kind=kint) :: nd, i
!
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do
        do i = 1, num
          elen_tgt%df_x2(i,nd) = elen_org%df_x2(i,nd)
          elen_tgt%df_y2(i,nd) = elen_org%df_y2(i,nd)
          elen_tgt%df_z2(i,nd) = elen_org%df_z2(i,nd)
          elen_tgt%df_xy(i,nd) = elen_org%df_xy(i,nd)
          elen_tgt%df_yz(i,nd) = elen_org%df_yz(i,nd)
          elen_tgt%df_zx(i,nd) = elen_org%df_zx(i,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_elen_diffs_type
!
!  ---------------------------------------------------------------------
!
      end module t_filter_elength
