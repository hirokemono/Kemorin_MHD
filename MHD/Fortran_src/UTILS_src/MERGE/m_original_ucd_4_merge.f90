!m_original_ucd_4_merge.f90
!      written by H. Matsui
!
!      subroutine allocate_subdomain_parameters
!      subroutine deallocate_subdomain_parameters
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
      type(phys_data) :: ucd
!
      integer(kind=kint ), allocatable :: ifield_2_copy(:)
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
       call alloc_phys_name_type(ucd)
!
       allocate ( ifield_2_copy(ucd%num_phys) )
       if(ucd%num_phys .eq. 0) ifield_2_copy =   0
!
       end subroutine allocate_subdomain_parameters
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_subdomain_parameters
!
!
       call dealloc_phys_name_type(ucd)
!
       deallocate ( ifield_2_copy)
!
       end subroutine deallocate_subdomain_parameters
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
        do  j = 1, ucd%num_phys
!          write(*,*) 'j,k',ucd_on_label(k),ucd%phys_name(j)
          if(ucd_on_label(k) .eq. ucd%phys_name(j) ) then
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
      do j = 1,ucd%num_phys
        if(ifield_2_copy(j) .gt. 0) then
          ic = ic + 1
          merged_fld%num_component(ic) =  ucd%num_component(j)
          merged_fld%phys_name(ic) = ucd%phys_name(j)
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
