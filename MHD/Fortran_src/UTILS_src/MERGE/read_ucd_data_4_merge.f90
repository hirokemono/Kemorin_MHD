!
!      module read_ucd_data_4_merge
!
!      Written by H. Matsui on Feb., 2007
!
!       subroutine init_ucd_data_4_merge(istep)
!       subroutine s_read_ucd_data_4_merge(istep, ifile_type)
!
      module read_ucd_data_4_merge
!
      use m_precision
      use m_ucd_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_ucd_data_4_merge(istep)
!
      use m_constants
      use m_control_param_merge
      use m_original_ucd_4_merge
      use ucd_IO_select
!
       integer (kind = kint), intent(in) :: istep
       integer (kind = kint) :: i
!
!
      nnod_ucd = ione
      itype_ucd_data_file = itype_org_ucd_file
      ucd_header_name = udt_original_header
      call sel_read_udt_param(izero, istep)
      call deallocate_ucd_phys_data
!
      ucd%num_phys =    num_field_ucd
      call allocate_subdomain_parameters
!
      ucd%istack_component(0) = 0
      do i = 1, ucd%num_phys
        ucd%num_component(i) =     num_comp_ucd(i)
        ucd%phys_name(i) =         phys_name_ucd(i)
        ucd%istack_component(i) =  ucd%istack_component(i-1)            &
     &                           + ucd%num_component(i)
      end do
!
      call deallocate_ucd_phys_name
!
      end subroutine init_ucd_data_4_merge
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_ucd_data_4_merge(istep)
!
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_original_ucd_4_merge
      use set_read_geometry_2_merge
      use ucd_IO_select
!
       integer (kind = kint), intent(in) :: istep
!
       integer (kind = kint) :: ip, my_rank
!
! ========================
! * PES loops 
! ========================
!
      num_field_ucd = ucd%num_phys
      ntot_comp_ucd = ucd%istack_component(ucd%num_phys)
      call allocate_ucd_phys_name
!
      itype_ucd_data_file = itype_org_ucd_file
      ucd_header_name = udt_original_header
!
      do ip =1, num_pe
        my_rank = ip - 1
        nnod_ucd = subdomain(ip)%node%numnod
        call allocate_ucd_phys_data
!
        call sel_read_udt_file(my_rank, istep)
!
        call copy_udt_field_data_merge(ip)
!
        call deallocate_ucd_phys_data
      end do
      call deallocate_ucd_phys_name
!
      end subroutine s_read_ucd_data_4_merge
!
! -----------------------------------------------------------------------
!
      end module read_ucd_data_4_merge
