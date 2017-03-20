!output_newdomain_ucd.f90
!      module output_newdomain_ucd
!
!      Written by H. Matsui on Feb., 2007
!
!!      subroutine assemble_2nd_udt_phys(istep, t_IO, ucd)
!!      subroutine assemble_2nd_udt_mesh(ucd)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!
      module output_newdomain_ucd
!
      use m_precision
!
      use m_constants
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use t_time_data
      use t_ucd_data
!
      implicit none
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
      subroutine assemble_2nd_udt_phys(istep, t_IO, ucd)
!
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer (kind = kint) :: ip, my_rank
!
!
      ucd%num_field = merged_fld%num_phys
      ucd%ntot_comp = merged_fld%ntot_phys
      call allocate_ucd_phys_name(ucd)
!
      ucd%num_comp(1:ucd%num_field)                                     &
     &     = merged_fld%num_component(1:ucd%num_field)
      ucd%phys_name(1:ucd%num_field)                                    &
     &     = merged_fld%phys_name(1:ucd%num_field)
!
      ucd%ifmt_file = itype_assembled_data
      ucd%file_prefix = new_udt_head
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        ucd%nnod = subdomains_2(ip)%node%numnod
        call allocate_ucd_node(ucd)
        call allocate_ucd_phys_data(ucd)
!
        call copy_domain_data_from_global(ip, ucd)
        call sel_write_udt_file(my_rank, istep, t_IO, ucd)
!
        call deallocate_ucd_phys_data(ucd)
        call deallocate_ucd_node(ucd)
      end do
      call deallocate_ucd_phys_name(ucd)
!
      end subroutine assemble_2nd_udt_phys
!
! -----------------------------------------------------------------------
!
      subroutine assemble_2nd_udt_mesh(ucd)
!
      use m_file_format_switch
      use set_and_cal_udt_data
      use ucd_IO_select
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: ip, my_rank
!
!
      ucd%ifmt_file = itype_assembled_data
      ucd%nnod_4_ele = merged%ele%nnod_4_ele
      ucd%file_prefix = new_udt_head
!
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        ucd%nnod = subdomains_2(ip)%node%numnod
        call allocate_ucd_node(ucd)
        call copy_node_posi_from_global(ip, ucd)
!
        call const_udt_global_connect                                   &
     &     (subdomains_2(ip)%node%internal_node,                        &
     &      subdomains_2(ip)%ele%numele,                                &
     &      subdomains_2(ip)%ele%nnod_4_ele,                            &
     &      subdomains_2(ip)%ele%iele_global, subdomains_2(ip)%ele%ie,  &
     &      ucd)
!
        call sel_write_grd_file(my_rank, ucd)
!
        call deallocate_ucd_node(ucd)
!
        if(   mod(ucd%ifmt_file,100)/10 .eq. iflag_vtd/10               &
       & .or. mod(ucd%ifmt_file,100)/10 .eq. iflag_udt/10) then
          call deallocate_ucd_ele(ucd)
        end if
      end do
!
      end subroutine assemble_2nd_udt_mesh
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
