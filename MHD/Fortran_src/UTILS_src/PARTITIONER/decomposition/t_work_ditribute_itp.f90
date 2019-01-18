!>@file   t_work_ditribute_itp.f90
!!@brief  module t_work_ditribute_itp
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Sep., 2013
!
!>@brief  Work array for distributed interpolation table
!!        for parallel domains
!!
!!@verbatim
!!      subroutine set_work_4_ditribute_itp(nprocs_org, nprocs_dest,    &
!!     &          femmesh_org, femmesh_dest, single_tbl, wk_dist_itp)
!!        type(mesh_data), intent(in) :: femmesh_org(nprocs_org)
!!        type(mesh_data), intent(in) :: femmesh_dest(nprocs_dest)
!!        type(interpolate_table), intent(in) :: single_tbl
!!        type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!!      subroutine dealloc_work_ditribute_itp(wk_dist_itp)
!!@endverbatim
!!
!!@n @param  nprocs_org    number of subdomains for original mesh
!!@n @param  nprocs_dest   number of subdomains for target mesh
!!@n @param  femmesh_org   mesh structure for original mesh
!!@n @param  femmesh_dest  mesh structure for target mesh
!!@n @param  single_tbl    interpolation table for single domain
!
      module t_work_ditribute_itp
!
      use m_precision
!
      use t_mesh_data
      use t_mesh_data_with_pointer
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
      type work_ditribute_itp
        integer(kind = kint) :: nprocs_itp_dest
        integer(kind = kint) :: nprocs_itp_org
!
        integer(kind = kint) :: ntot_nod_dest
        integer(kind = kint) :: ntot_ele_org
!
        integer(kind = kint), allocatable :: iele_lc_org(:,:)
        integer(kind = kint), allocatable :: inod_lc_dest(:,:)
!
        integer(kind = kint), allocatable :: ntable_para(:)
        integer(kind = kint), allocatable :: istack_para(:)
        integer(kind = kint), allocatable :: itable_para_order(:)
        integer(kind = kint), allocatable :: irev_tbl_org(:)
      end type work_ditribute_itp
!
      private :: alloc_work_ditribute_itp
      private :: count_local_nod_ele_2_global
      private :: set_local_nod_ele_2_global
      private :: count_ordering_4_parallel_itp
      private :: set_ordering_4_parallel_itp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_work_4_ditribute_itp(nprocs_org, nprocs_dest,      &
     &          femmesh_org, femmesh_dest, single_tbl, wk_dist_itp)
!
      integer(kind = kint), intent(in) :: nprocs_org
      type(mesh_data), intent(in) :: femmesh_org(nprocs_org)
      integer(kind = kint), intent(in) :: nprocs_dest
      type(mesh_data), intent(in) :: femmesh_dest(nprocs_dest)
      type(interpolate_table), intent(in) :: single_tbl
!
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
!
      wk_dist_itp%nprocs_itp_dest = nprocs_dest
      wk_dist_itp%nprocs_itp_org =  nprocs_org
!
      call count_local_nod_ele_2_global(nprocs_org, nprocs_dest,        &
     &    femmesh_org, femmesh_dest, wk_dist_itp)
!
      if(single_tbl%tbl_org%ntot_table_org                              &
     & .ne. single_tbl%tbl_dest%ntot_table_dest) then
        write(*,*) 'something is wrong in the table'
        stop
      end if
      if(single_tbl%tbl_org%ntot_table_org                              &
     &       .ne. wk_dist_itp%ntot_nod_dest) then
        write(*,*) 'something is wrong in the table'
        stop
      end if
!
      call alloc_work_ditribute_itp                                     &
     &   (nprocs_org, nprocs_dest, wk_dist_itp)
!
      call set_local_nod_ele_2_global(nprocs_org, nprocs_dest,          &
     &    femmesh_org, femmesh_dest, wk_dist_itp)
!
      call count_ordering_4_parallel_itp(single_tbl, wk_dist_itp)
      call set_ordering_4_parallel_itp(single_tbl, wk_dist_itp)
!
      end subroutine set_work_4_ditribute_itp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_work_ditribute_itp(wk_dist_itp)
!
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
!
      deallocate(wk_dist_itp%iele_lc_org, wk_dist_itp%inod_lc_dest)
!
      deallocate(wk_dist_itp%ntable_para, wk_dist_itp%istack_para)
      deallocate(wk_dist_itp%itable_para_order)
      deallocate(wk_dist_itp%irev_tbl_org)
!
      end subroutine dealloc_work_ditribute_itp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_work_ditribute_itp                               &
     &         (nprocs_org, nprocs_dest, wk_dist_itp)
!
      integer(kind = kint), intent(in) :: nprocs_org, nprocs_dest
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
!
      allocate(wk_dist_itp%iele_lc_org(wk_dist_itp%ntot_ele_org,2))
      allocate(wk_dist_itp%inod_lc_dest(wk_dist_itp%ntot_nod_dest,2))
!
      allocate(wk_dist_itp%ntable_para(4*nprocs_org*nprocs_dest))
      allocate(wk_dist_itp%istack_para(0:4*nprocs_org*nprocs_dest))
      allocate(wk_dist_itp%itable_para_order(wk_dist_itp%ntot_nod_dest))
      allocate(wk_dist_itp%irev_tbl_org(wk_dist_itp%ntot_nod_dest))
!
      wk_dist_itp%istack_para = 0
      if(wk_dist_itp%ntot_ele_org .gt. 0) wk_dist_itp%iele_lc_org = 0
      if(nprocs_org*nprocs_dest .gt. 0) wk_dist_itp%ntable_para = 0
      if(wk_dist_itp%ntot_nod_dest .gt. 0) then
        wk_dist_itp%inod_lc_dest = 0
        wk_dist_itp%itable_para_order = 0
        wk_dist_itp%irev_tbl_org = 0
      end if
!
      end subroutine alloc_work_ditribute_itp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_local_nod_ele_2_global(nprocs_org, nprocs_dest,  &
     &          femmesh_org, femmesh_dest, wk_dist_itp)
!
      integer(kind = kint), intent(in) :: nprocs_org
      type(mesh_data), intent(in) :: femmesh_org(nprocs_org)
      integer(kind = kint), intent(in) :: nprocs_dest
      type(mesh_data), intent(in) :: femmesh_dest(nprocs_dest)
!
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
      integer(kind = kint) :: ip, jp
!
!
      wk_dist_itp%ntot_nod_dest = 0
      do ip = 1, nprocs_dest
        wk_dist_itp%ntot_nod_dest = wk_dist_itp%ntot_nod_dest           &
     &                + femmesh_dest(ip)%mesh%node%internal_node
      end do
!
      wk_dist_itp%ntot_ele_org = 0
      do jp = 1, nprocs_org
        wk_dist_itp%ntot_ele_org = wk_dist_itp%ntot_ele_org             &
     &                + femmesh_org(jp)%mesh%ele%internal_ele
      end do
!
      end subroutine count_local_nod_ele_2_global
!
!-----------------------------------------------------------------------
!
      subroutine set_local_nod_ele_2_global(nprocs_org, nprocs_dest,    &
     &          femmesh_org, femmesh_dest, wk_dist_itp)
!
      integer(kind = kint), intent(in) :: nprocs_org
      type(mesh_data), intent(in) :: femmesh_org(nprocs_org)
      integer(kind = kint), intent(in) :: nprocs_dest
      type(mesh_data), intent(in) :: femmesh_dest(nprocs_dest)
!
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
      integer(kind = kint) :: ip, jp, inod, jele, iflag
      integer(kind = kint_gl) :: inod_gl, jele_gl
!
!
      do ip = 1, nprocs_dest
        do inod = 1, femmesh_dest(ip)%mesh%node%internal_node
          inod_gl =  femmesh_dest(ip)%mesh%node%inod_global(inod)
          wk_dist_itp%inod_lc_dest(inod_gl,1) = inod
          wk_dist_itp%inod_lc_dest(inod_gl,2) = ip
        end do
      end do
!
      do jp = 1, nprocs_org
        do jele = 1, femmesh_org(jp)%mesh%ele%numele
          if(femmesh_org(jp)%mesh%ele%ie(jele,1)                        &
     &      .gt. femmesh_org(jp)%mesh%node%internal_node) cycle
          jele_gl =  femmesh_org(jp)%mesh%ele%iele_global(jele)
          wk_dist_itp%iele_lc_org(jele_gl,1) = jele
          wk_dist_itp%iele_lc_org(jele_gl,2) = jp
        end do
      end do
!
      do inod = 1, wk_dist_itp%ntot_nod_dest
        iflag = wk_dist_itp%inod_lc_dest(inod,1)                        &
     &         * wk_dist_itp%inod_lc_dest(inod,2)
        if(iflag .eq. 0) then
          write(*,*) 'node ', inod,                                     &
     &               ' is missing: ', wk_dist_itp%inod_lc_dest(inod,:)
        end if
      end do
      do jele = 1, wk_dist_itp%ntot_ele_org
        iflag = wk_dist_itp%iele_lc_org(jele,1)                         &
     &         *  wk_dist_itp%iele_lc_org(jele,2)
        if(iflag .eq. 0) then
          write(*,*) 'element ', jele,                                  &
     &             ' is missing: ', wk_dist_itp%iele_lc_org(jele,:)
        end if
      end do
!
      end subroutine set_local_nod_ele_2_global
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_ordering_4_parallel_itp(single_tbl, wk_dist_itp)
!
      type(interpolate_table), intent(in) :: single_tbl
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
      integer(kind = kint) :: i, ist, ied, inum
      integer(kind = kint) :: inod_gl, jele_gl,  ip, jp, k
!
!
      wk_dist_itp%ntable_para = 0
      do i = 1, 4
        ist = single_tbl%tbl_org%istack_itp_type_org(i-1) + 1
        ied = single_tbl%tbl_org%istack_itp_type_org(i)
        do inum  = ist, ied
          inod_gl = single_tbl%tbl_dest%inod_dest_4_dest(inum)
          jele_gl = single_tbl%tbl_org%iele_org_4_org(inum)
          ip = wk_dist_itp%inod_lc_dest(inod_gl,2)
          jp = wk_dist_itp%iele_lc_org(jele_gl,2)
          k = i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          wk_dist_itp%ntable_para(k) = wk_dist_itp%ntable_para(k) + 1
        end do
      end do
!
      do ip = 1, wk_dist_itp%nprocs_itp_dest
        do jp = 1, wk_dist_itp%nprocs_itp_org
          do i = 1, 4
            k = i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
            wk_dist_itp%istack_para(k) = wk_dist_itp%istack_para(k-1)   &
     &                                  + wk_dist_itp%ntable_para(k)
          end do
        end do
      end do
!
      end subroutine count_ordering_4_parallel_itp
!
!-----------------------------------------------------------------------
!
      subroutine set_ordering_4_parallel_itp(single_tbl, wk_dist_itp)
!
      type(interpolate_table), intent(in) :: single_tbl
      type(work_ditribute_itp), intent(inout) :: wk_dist_itp
!
      integer(kind = kint) :: i, ist, ied, inum, ic, ip, jp, k
      integer(kind = kint) :: inod_gl, jele_gl
!
!
      wk_dist_itp%ntable_para = 0
      do i = 1, 4
        ist = single_tbl%tbl_org%istack_itp_type_org(i-1) + 1
        ied = single_tbl%tbl_org%istack_itp_type_org(i)
        do inum  = ist, ied
          inod_gl = single_tbl%tbl_dest%inod_dest_4_dest(inum)
          jele_gl = single_tbl%tbl_org%iele_org_4_org(inum)
          ip = wk_dist_itp%inod_lc_dest(inod_gl,2)
          jp = wk_dist_itp%iele_lc_org(jele_gl,2)
          k = i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          wk_dist_itp%ntable_para(k) = wk_dist_itp%ntable_para(k) + 1
          ic = wk_dist_itp%istack_para(k-1)                             &
     &        + wk_dist_itp%ntable_para(k)
          wk_dist_itp%itable_para_order(ic) = inod_gl
        end do
      end do
!
      do inum = 1, single_tbl%tbl_org%ntot_table_org
        inod_gl = single_tbl%tbl_org%inod_gl_dest_4_org(inum)
        wk_dist_itp%irev_tbl_org(inod_gl) = inum
      end do
!
      end subroutine set_ordering_4_parallel_itp
!
!-----------------------------------------------------------------------
!
      end module t_work_ditribute_itp
