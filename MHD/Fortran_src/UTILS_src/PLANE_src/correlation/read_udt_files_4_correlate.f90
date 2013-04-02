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
      itype_ucd_data_file = itype_cor_ucd_file
      ucd_header_name =     cor_udt_header
      nnod_ucd = ione
      call sel_read_udt_param(izero, istep)
      call deallocate_ucd_phys_data
!
      cor_ucd%num_phys = num_field_ucd
      call alloc_phys_name_type(cor_ucd)
!
      cor_ucd%num_component(1:cor_ucd%num_phys)                         &
     &             = num_comp_ucd(1:cor_ucd%num_phys)
      cor_ucd%phys_name(1:cor_ucd%num_phys)                             &
     &             = phys_name_ucd(1:cor_ucd%num_phys)
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
      itype_ucd_data_file = itype_cor_ucd_file
      nnod_ucd = merge_tbl%nnod_max
      call allocate_ucd_phys_data
      do ip =1, num_pe
        my_rank = ip - 1
!
        nnod_ucd = subdomain(ip)%node%numnod
        ucd_header_name = cor_udt_header
        call sel_read_udt_file(my_rank, istep)
!
        call copy_and_pick_udt_data_merge                               &
     &    (subdomain(ip)%node%numnod, subdomain(ip)%node%internal_node, &
     &      num_domain, subdomain(ip)%node%inod_global, num_crt,        &
     &      icomp_crt, ifield_crt, phys_d1(1))
      end do
      call deallocate_ucd_phys_data
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
      itype_ucd_data_file = itype_ref_ucd_file
      ucd_header_name =     ref_udt_header
      nnod_ucd = ione
      call sel_read_udt_param(izero, istep)
      call deallocate_ucd_phys_data
!
      ref_ucd%num_phys =    num_field_ucd
      call alloc_phys_name_type(ref_ucd)
!
      ref_ucd%istack_component(0) = 0
      do i = 1, ref_ucd%num_phys
        ref_ucd%phys_name(i) =      phys_name_ucd(i)
        ref_ucd%num_component(i) =  num_comp_ucd(i)
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
      itype_ucd_data_file = itype_ref_ucd_file
      nnod_ucd = merge_tbl_2%nnod_max
      call allocate_ucd_phys_data
      do ip =1, num_pe2
        my_rank = ip - 1
!
        nnod_ucd = subdomains_2(ip)%node%numnod
        ucd_header_name = ref_udt_header
        call sel_read_udt_file(my_rank, istep)
!
        call copy_and_pick_udt_data_merge                              &
     &      (subdomains_2(ip)%node%numnod,                             &
     &       subdomains_2(ip)%node%internal_node, num_domain,          &
     &       subdomains_2(ip)%node%inod_global, num_crt, icomp_crt,    &
     &      ifield_crt2, phys_d2(1))
      end do
      call deallocate_ucd_phys_data
!
      end subroutine read_2nd_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      end module read_udt_files_4_correlate
