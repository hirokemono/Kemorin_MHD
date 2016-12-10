!output_newdomain_ucd.f90
!      module output_newdomain_ucd
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine assemble_2nd_udt_phys(istep)
!      subroutine assemble_2nd_udt_nesh
!
      module output_newdomain_ucd
!
      use m_precision
!
      use m_constants
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_time_data_IO
      use t_ucd_data
!
      implicit none
!
      type(ucd_data), save, private :: fem_ucd
!
      private :: copy_domain_data_from_global
      private :: copy_node_posi_from_global
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine assemble_2nd_udt_phys(istep)
!
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, my_rank
!
!
      fem_ucd%num_field = merged_fld%num_phys
      fem_ucd%ntot_comp = merged_fld%ntot_phys
      call allocate_ucd_phys_name(fem_ucd)
!
      fem_ucd%num_comp(1:fem_ucd%num_field)                             &
     &     = merged_fld%num_component(1:fem_ucd%num_field)
      fem_ucd%phys_name(1:fem_ucd%num_field)                            &
     &     = merged_fld%phys_name(1:fem_ucd%num_field)
!
      fem_ucd%ifmt_file = itype_assembled_data
      fem_ucd%file_prefix = new_udt_head
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        fem_ucd%nnod = subdomains_2(ip)%node%numnod
        call allocate_ucd_node(fem_ucd)
        call allocate_ucd_phys_data(fem_ucd)
!
        call copy_domain_data_from_global(ip, fem_ucd)
        call sel_write_udt_file(my_rank, istep, t1_IO, fem_ucd)
!
        call deallocate_ucd_phys_data(fem_ucd)
        call deallocate_ucd_node(fem_ucd)
      end do
      call deallocate_ucd_phys_name(fem_ucd)
!
      end subroutine assemble_2nd_udt_phys
!
! -----------------------------------------------------------------------
!
      subroutine assemble_2nd_udt_nesh
!
      use m_file_format_switch
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint) :: ip, my_rank
!
!
      fem_ucd%ifmt_file = itype_assembled_data
      fem_ucd%nnod_4_ele = merged%ele%nnod_4_ele
      fem_ucd%file_prefix = new_udt_head
!
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        fem_ucd%nnod = subdomains_2(ip)%node%numnod
        call allocate_ucd_node(fem_ucd)
        call copy_node_posi_from_global(ip, fem_ucd)
!
        call const_udt_global_connect                                   &
     &     (subdomains_2(ip)%node%internal_node,                        &
     &      subdomains_2(ip)%ele%numele,                                &
     &      subdomains_2(ip)%ele%nnod_4_ele,                            &
     &      subdomains_2(ip)%ele%iele_global, subdomains_2(ip)%ele%ie,  &
     &      fem_ucd)
!
        call sel_write_grd_file(my_rank, fem_ucd)
!
        call deallocate_ucd_node(fem_ucd)
!
        if(   mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_vtd/10           &
       & .or. mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_udt/10) then
          call deallocate_ucd_ele(fem_ucd)
        end if
      end do
!
      end subroutine assemble_2nd_udt_nesh
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_domain_data_from_global(ip, ucd)
!
      integer(kind = kint), intent(in) :: ip
      type(ucd_data), intent(inout) :: ucd
!
      integer (kind = kint_gl) :: inum
      integer (kind = kint_gl) :: inod
!
!
      do inum = 1, subdomains_2(ip)%node%numnod
        inod = subdomains_2(ip)%node%inod_global(inum)
        ucd%inod_global(inum) = inod
        if(int(inod) .le. merged%node%numnod) then
          ucd%d_ucd(inum,1:ucd%ntot_comp)                               &
     &        = merged_fld%d_fld(inod,1:ucd%ntot_comp)
        else
          ucd%d_ucd(inum,1:ucd%ntot_comp) = zero
        end if
      end do
!
      end subroutine copy_domain_data_from_global
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_posi_from_global(ip, ucd)
!
      integer(kind = kint), intent(in) :: ip
      type(ucd_data), intent(inout) :: ucd
!
      integer (kind = kint_gl) :: inum
      integer (kind = kint_gl) :: inod
!
!
      do inum = 1, ucd%nnod
        inod = subdomains_2(ip)%node%inod_global(inum)
        ucd%inod_global(inum) = inod
        ucd%xx(inum,1:3) = merged%node%xx(inod,1:3)
      end do
!
      end subroutine copy_node_posi_from_global
!
!  ---------------------------------------------------------------------
!
      end module output_newdomain_ucd
