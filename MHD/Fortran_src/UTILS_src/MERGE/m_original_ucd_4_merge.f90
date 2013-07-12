!m_original_ucd_4_merge.f90
!      written by H. Matsui
!
!      subroutine allocate_subdomain_parameters
!      subroutine deallocate_subdomain_parameters
!
!      subroutine init_ucd_data_4_merge(istep)
!      subroutine read_ucd_data_4_merge(istep)
!
!      subroutine set_field_list_4_merge
!
      module m_original_ucd_4_merge
!
      use m_precision
      use t_phys_data
!
      implicit none
!
!
      type(phys_data), save, private :: org_fld
      integer(kind=kint ), allocatable, private :: ifield_2_copy(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_subdomain_parameters
!
!
       call alloc_phys_name_type(org_fld)
!
       allocate ( ifield_2_copy(org_fld%num_phys) )
       if(org_fld%num_phys .eq. 0) ifield_2_copy =   0
!
       end subroutine allocate_subdomain_parameters
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_subdomain_parameters
!
!
       call dealloc_phys_name_type(org_fld)
!
       deallocate ( ifield_2_copy)
!
       end subroutine deallocate_subdomain_parameters
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_ucd_data_4_merge(istep)
!
      use m_ucd_data
      use m_constants
      use m_control_param_merge
      use ucd_IO_select
!
       integer (kind = kint), intent(in) :: istep
       integer (kind = kint) :: i
!
!
      fem_ucd%nnod = ione
      fem_ucd%ifmt_file = itype_org_ucd_file
      fem_ucd%file_prefix = udt_original_header
      call sel_read_udt_param(izero, istep, fem_ucd)
      call deallocate_ucd_phys_data(fem_ucd)
!
      org_fld%num_phys =    fem_ucd%num_field
      call allocate_subdomain_parameters
!
      org_fld%istack_component(0) = 0
      do i = 1, org_fld%num_phys
        org_fld%num_component(i) =     fem_ucd%num_comp(i)
        org_fld%phys_name(i) =         fem_ucd%phys_name(i)
        org_fld%istack_component(i) = org_fld%istack_component(i-1)     &
     &                              + org_fld%num_component(i)
      end do
!
      call deallocate_ucd_phys_name(fem_ucd)
!
      end subroutine init_ucd_data_4_merge
!
!  ---------------------------------------------------------------------
!
      subroutine read_ucd_data_4_merge(istep)
!
      use m_ucd_data
      use m_control_param_merge
      use m_geometry_data_4_merge
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
      fem_ucd%num_field = org_fld%num_phys
      fem_ucd%ntot_comp = org_fld%istack_component(org_fld%num_phys)
      call allocate_ucd_phys_name(fem_ucd)
!
      fem_ucd%ifmt_file = itype_org_ucd_file
      fem_ucd%file_prefix = udt_original_header
!
      do ip =1, num_pe
        my_rank = ip - 1
        fem_ucd%nnod = subdomain(ip)%node%numnod
        call allocate_ucd_phys_data(fem_ucd)
!
        call sel_read_udt_file(my_rank, istep, fem_ucd)
!
        call copy_udt_field_data_merge(ip, org_fld, ifield_2_copy)
!
        call deallocate_ucd_phys_data(fem_ucd)
      end do
      call deallocate_ucd_phys_name(fem_ucd)
!
      end subroutine read_ucd_data_4_merge
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_field_list_4_merge
!
      use m_control_param_merge
      use m_geometry_data_4_merge
!
      integer(kind=kint) :: j, k, ic
!
!
!    set list array for merged field
!
      ic = 0 
      ifield_2_copy = 0
      do  k = 1, num_nod_phys
        do  j = 1, org_fld%num_phys
!          write(*,*) 'j,k',ucd_on_label(k),org_fld%phys_name(j)
          if(ucd_on_label(k) .eq. org_fld%phys_name(j) ) then
             ic = ic + 1
             ifield_2_copy(j) = k
             exit
          end if
        end do
      end do
!
      merged_fld%num_phys =  ic
      merged_fld%num_phys_viz =  merged_fld%num_phys
      call allocate_merged_field_name
!
      ic =  0 
      do j = 1,org_fld%num_phys
        if(ifield_2_copy(j) .gt. 0) then
          ic = ic + 1
          merged_fld%num_component(ic) =  org_fld%num_component(j)
          merged_fld%phys_name(ic) =      org_fld%phys_name(j)
        end if
      end do
!
      merged_fld%istack_component(0) = 0
      do j = 1, merged_fld%num_phys
        merged_fld%istack_component(j)                                  &
     &             = merged_fld%istack_component(j-1)                   &
     &              + merged_fld%num_component(j)
      end do
      merged_fld%ntot_phys                                              &
     &     =  merged_fld%istack_component(merged_fld%num_phys)
!
!      write(*,*) 'subdomain field name list'
      call check_nodal_field_name_type(merged_fld)
!
      end subroutine set_field_list_4_merge
!
!  ---------------------------------------------------------------------
!
      end module m_original_ucd_4_merge
