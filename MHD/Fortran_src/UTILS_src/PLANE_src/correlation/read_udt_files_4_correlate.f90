!
!      module read_udt_files_4_correlate
!
!     Written by H. Matsui on Feb., 2007
!
!      subroutine init_udt_4_correlate(istep)
!      subroutine read_udt_4_correlate(istep)
!
!      subroutine init_2nd_udt_4_correlate(istep)
!      subroutine read_2nd_udt_4_correlate(istep)
!
      module read_udt_files_4_correlate
!
      use m_precision
!
      use m_constants
      use m_ucd_data
      use m_correlate_4_plane
!
      use copy_pick_udt_data_plane
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_udt_4_correlate(istep)
!
      use m_geometry_data_4_merge
      use m_file_format_switch
      use t_phys_data
      use ucd_IO_select
      use cal_minmax_and_stacks
!
      integer (kind = kint), intent(in) :: istep
!
!
      fem_ucd%ifmt_file =   itype_cor_ucd_file
      fem_ucd%file_prefix = cor_udt_header
      fem_ucd%nnod =        ione
      call sel_read_udt_param(izero, istep, fem_ucd)
      call deallocate_ucd_phys_data(fem_ucd)
!
      cor_ucd%num_phys = fem_ucd%num_field
      call alloc_phys_name_type(cor_ucd)
!
      cor_ucd%num_component(1:cor_ucd%num_phys)                         &
     &             = fem_ucd%num_comp(1:cor_ucd%num_phys)
      cor_ucd%phys_name(1:cor_ucd%num_phys)                             &
     &             = fem_ucd%phys_name(1:cor_ucd%num_phys)
!
      call s_cal_total_and_stacks(cor_ucd%num_phys,                     &
     &    cor_ucd%num_component, izero, cor_ucd%istack_component,       &
     &    cor_ucd%ntot_phys)
!
      end subroutine init_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      subroutine read_udt_4_correlate(istep)
!
      use m_geometry_data_4_merge
      use m_file_format_switch
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, my_rank
!
!
! ========================
! * PES loops 
! ========================
!
      fem_ucd%ifmt_file = itype_cor_ucd_file
      fem_ucd%nnod = merge_tbl%nnod_max
      call allocate_ucd_phys_data(fem_ucd)
      do ip =1, num_pe
        my_rank = ip - 1
!
        fem_ucd%nnod =        subdomain(ip)%node%numnod
        fem_ucd%file_prefix = cor_udt_header
        call sel_read_udt_file(my_rank, istep, fem_ucd)
!
        call copy_and_pick_udt_data_merge                               &
     &    (subdomain(ip)%node%numnod, subdomain(ip)%node%internal_node, &
     &      num_domain, subdomain(ip)%node%inod_global, num_crt,        &
     &      icomp_crt, ifield_crt, phys_d1(1))
      end do
      call deallocate_ucd_phys_data(fem_ucd)
!
      end subroutine read_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      subroutine init_2nd_udt_4_correlate(istep)
!
       use m_2nd_geometry_4_merge
       use m_file_format_switch
       use t_phys_data
       use ucd_IO_select
!
       integer (kind = kint), intent(in) :: istep
       integer (kind = kint) :: i
!
!
      fem_ucd%ifmt_file =   itype_ref_ucd_file
      fem_ucd%file_prefix = ref_udt_header
      fem_ucd%nnod = ione
      call sel_read_udt_param(izero, istep, fem_ucd)
      call deallocate_ucd_phys_data(fem_ucd)
!
      ref_ucd%num_phys =    fem_ucd%num_field
      call alloc_phys_name_type(ref_ucd)
!
      ref_ucd%istack_component(0) = 0
      do i = 1, ref_ucd%num_phys
        ref_ucd%phys_name(i) =      fem_ucd%phys_name(i)
        ref_ucd%num_component(i) =  fem_ucd%num_comp(i)
        ref_ucd%istack_component(i) = ref_ucd%istack_component(i-1)     &
     &                              + ref_ucd%num_component(i)
      end do
!
      end subroutine init_2nd_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      subroutine read_2nd_udt_4_correlate(istep)
!
       use m_2nd_geometry_4_merge
       use m_file_format_switch
       use ucd_IO_select
!
       integer (kind = kint), intent(in) :: istep
       integer (kind = kint) :: ip, my_rank
!
!
      fem_ucd%ifmt_file = itype_ref_ucd_file
      fem_ucd%nnod = merge_tbl_2%nnod_max
      call allocate_ucd_phys_data(fem_ucd)
      do ip =1, num_pe2
        my_rank = ip - 1
!
        fem_ucd%nnod =        subdomains_2(ip)%node%numnod
        fem_ucd%file_prefix = ref_udt_header
        call sel_read_udt_file(my_rank, istep, fem_ucd)
!
        call copy_and_pick_udt_data_merge                              &
     &      (subdomains_2(ip)%node%numnod,                             &
     &       subdomains_2(ip)%node%internal_node, num_domain,          &
     &       subdomains_2(ip)%node%inod_global, num_crt, icomp_crt,    &
     &      ifield_crt2, phys_d2(1))
      end do
      call deallocate_ucd_phys_data(fem_ucd)
!
      end subroutine read_2nd_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      end module read_udt_files_4_correlate
