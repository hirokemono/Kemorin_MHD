!t_source_of_filed_line.f90
!
!      module t_source_of_filed_line
!
!      Written by H. Matsui on Aug., 2011
!
!      subroutine allocate_local_start_grp_num
!      subroutine allocate_local_start_grp_item
!      subroutine allocate_local_data_4_fline(numnod)
!      subroutine allocate_start_point_fline
!!      subroutine alloc_num_gl_start_fline                             &
!!     &         (nprocs, num_fline, ntot_each_field_line, fline_trc)
!      subroutine deallocate_local_data_4_fline
!      subroutine deallocate_local_start_grp_item
!      subroutine deallocate_start_point_fline
!!      subroutine dealloc_num_gl_start_fline(fline_trc)
!
      module t_source_of_filed_line
!
      use m_precision
!
      implicit  none
!
!
      real(kind = kreal), allocatable :: vector_nod_fline(:,:,:)
      real(kind = kreal), allocatable :: color_nod_fline(:,:)
!
      integer(kind = kint) :: ntot_ele_start_grp
      integer(kind = kint), allocatable :: istack_ele_start_grp(:)
      integer(kind = kint), allocatable :: nele_start_grp(:)
      integer(kind = kint), allocatable :: iele_start_item(:,:)
      real(kind = kreal),   allocatable :: flux_start(:)
!
      integer(kind = kint), allocatable :: num_line_local(:)
!
      real(kind = kreal),   allocatable :: xx_start_fline(:,:)
      real(kind = kreal),   allocatable :: flux_start_fline(:)
!
        integer(kind = kint) :: ntot_gl_fline = 0
!
      type fieldline_trace
        integer(kind = kint), allocatable :: istack_all_fline(:,:)
        integer(kind = kint), allocatable :: num_all_fline(:,:)
        real(kind = kreal),   allocatable :: flux_stack_fline(:)
!
        integer(kind= kint), allocatable :: iflag_fline(:)
        integer(kind= kint), allocatable :: icount_fline(:)
        integer(kind= kint), allocatable :: isf_fline_start(:,:)
        real(kind = kreal), allocatable ::  xx_fline_start(:,:)
        real(kind = kreal), allocatable ::  v_fline_start(:,:)
        real(kind = kreal), allocatable ::  c_fline_start(:)
!
        integer(kind= kint), allocatable :: id_fline_export(:,:)
        real(kind = kreal), allocatable ::  fline_export(:,:)
!
        integer(kind= kint), allocatable :: isf_fline_global(:,:)
        real(kind = kreal), allocatable ::  fline_global(:,:)
      end type fieldline_trace
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_start_grp_num
!
      use m_control_params_4_fline
!
!
      allocate(nele_start_grp(num_fline))
      allocate(istack_ele_start_grp(0:num_fline))
!
      allocate(num_line_local(num_fline))
!
      nele_start_grp =       0
      istack_ele_start_grp = 0
!
      num_line_local = 0
!
      end subroutine allocate_local_start_grp_num
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_start_grp_item
!
      use m_control_params_4_fline
!
!
      ntot_ele_start_grp = istack_ele_start_grp(num_fline)
      allocate(iele_start_item(2,ntot_ele_start_grp))
      allocate(flux_start(ntot_ele_start_grp))
      if(ntot_ele_start_grp .gt. 0) then
        iele_start_item =    0
        ntot_ele_start_grp = 0
      end if
!
      end subroutine allocate_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_data_4_fline(numnod)
!
      use m_control_params_4_fline
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(vector_nod_fline(numnod,3,num_fline))
      allocate(color_nod_fline(numnod,num_fline))
!
      vector_nod_fline = 0.0d0
      color_nod_fline =  0.0d0
!
      end subroutine allocate_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_start_point_fline
!
      use m_control_params_4_fline
!
!
      allocate(xx_start_fline(3,ntot_each_field_line))
      allocate(flux_start_fline(ntot_each_field_line))
!
      xx_start_fline = 0.0d0
      flux_start_fline =  0.0d0
!
      end subroutine allocate_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_gl_start_fline                               &
     &         (nprocs, num_fline, ntot_each_field_line, fline_trc)
!
      integer(kind = kint), intent(in) :: num_fline, nprocs
      integer(kind = kint), intent(in) :: ntot_each_field_line
      type(fieldline_trace), intent(inout) :: fline_trc
      integer(kind = kint) :: num
!
!
      allocate(fline_trc%istack_all_fline(0:nprocs,num_fline))
      allocate(fline_trc%num_all_fline(nprocs,num_fline))
      allocate(fline_trc%flux_stack_fline(0:nprocs))
!
      num = 2*ntot_each_field_line
      allocate(fline_trc%icount_fline(num))
      allocate(fline_trc%iflag_fline(num))
      allocate(fline_trc%isf_fline_start(3,num))
      allocate(fline_trc%xx_fline_start(3,num))
      allocate(fline_trc%v_fline_start(3,num))
      allocate(fline_trc%c_fline_start(num))
!
      fline_trc%istack_all_fline = 0
      fline_trc%num_all_fline =    0
      fline_trc%flux_stack_fline = 0.0d0
!
      fline_trc%icount_fline = 0
      fline_trc%iflag_fline =  0
      fline_trc%isf_fline_start = 0
      fline_trc%xx_fline_start = 0.0d0
      fline_trc%v_fline_start =  0.0d0
      fline_trc%c_fline_start =  0.0d0
!
      allocate(fline_trc%id_fline_export(7,num))
      allocate(fline_trc%fline_export(7,num))
      fline_trc%id_fline_export = 0
      fline_trc%fline_export = 0.0d0
!
      end subroutine alloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_start_grp_item
!
      deallocate(nele_start_grp, istack_ele_start_grp)
      deallocate(iele_start_item, flux_start)
      deallocate(num_line_local)
!
      end subroutine deallocate_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_data_4_fline
!
      deallocate(vector_nod_fline, color_nod_fline)
!
      end subroutine deallocate_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_start_point_fline
!
!
      deallocate(xx_start_fline, flux_start_fline)
!
      end subroutine deallocate_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_gl_start_fline(fline_trc)
!
      type(fieldline_trace), intent(inout) :: fline_trc
!
!
      deallocate(fline_trc%istack_all_fline)
      deallocate(fline_trc%num_all_fline)
      deallocate(fline_trc%flux_stack_fline)
!
      deallocate(fline_trc%icount_fline)
      deallocate(fline_trc%iflag_fline)
      deallocate(fline_trc%isf_fline_start)
      deallocate(fline_trc%xx_fline_start)
      deallocate(fline_trc%v_fline_start)
      deallocate(fline_trc%c_fline_start)
!
      deallocate(fline_trc%id_fline_export)
      deallocate(fline_trc%fline_export)
!
      end subroutine dealloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
