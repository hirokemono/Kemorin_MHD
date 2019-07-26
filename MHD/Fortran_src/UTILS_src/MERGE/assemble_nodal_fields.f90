!>@file   assemble_nodal_fields.f90
!!@brief  module assemble_nodal_fields
!!
!!@author H. Okuda and H. Matsui
!!@date  Programmed by H. MAtsui in June, 2018
!
!>@brief  Assemble nodal field data
!!
!!@verbatim
!!      subroutine alloc_assemble_field_list(asbl_tbl)
!!      subroutine dealloc_assemble_field_list(asbl_tbl)
!!      subroutine init_field_name_4_assemble_ucd                       &
!!     &         (asbl_tbl, fld_IO, fld)
!!
!!      subroutine assemble_field_data                                  &
!!     &         (nprocs_org, asbl_comm, new_fld, t_IO, org_fIO)
!!        type(comm_table_4_assemble), intent(in) :: asbl_comm
!!        type(phys_data), intent(inout) :: new_fld
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: org_fIO(nprocs_org)
!!@endverbatim
!!
      module assemble_nodal_fields
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_field_data_IO
!
      implicit none
!
      type assemble_field_list
        integer(kind=kint ) :: nfld_label
        character(len=kchara), allocatable :: ucd_on_label(:)
      end type assemble_field_list
!
      private :: count_fields_4_assemble_ucd
      private :: set_field_name_4_assemble_ucd
!      private :: copy_field_data_4_assemble
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_assemble_field_list(asbl_tbl)
!
      type(assemble_field_list), intent(inout) :: asbl_tbl
!
!
       allocate ( asbl_tbl%ucd_on_label(asbl_tbl%nfld_label) )
!
      end subroutine alloc_assemble_field_list
!
!------------------------------------------------------------------
!
      subroutine dealloc_assemble_field_list(asbl_tbl)
!
      type(assemble_field_list), intent(inout) :: asbl_tbl
!
       deallocate ( asbl_tbl%ucd_on_label )
!
      end subroutine dealloc_assemble_field_list
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine init_field_name_4_assemble_ucd                         &
     &         (asbl_tbl, fld_IO, fld)
!
      use cal_minmax_and_stacks
!
      type(field_IO), intent(in) :: fld_IO
      type(assemble_field_list), intent(in) :: asbl_tbl
!
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: ifld, jfld
!
!
      if(iflag_debug .eq. 0) then
        do jfld = 1, fld_IO%num_field_IO
          write(*,*) 'fld_IO', jfld, trim(fld_IO%fld_name(jfld))
        end do
      end if
!
      call count_fields_4_assemble_ucd                                  &
     &   (asbl_tbl%nfld_label, asbl_tbl%ucd_on_label,                   &
     &    fld_IO, fld%num_phys)
      call alloc_phys_name_type(fld)
!
      call set_field_name_4_assemble_ucd                                &
     &   (asbl_tbl%nfld_label, asbl_tbl%ucd_on_label, fld_IO, fld)
      call s_cal_total_and_stacks(fld%num_phys, fld%num_component,      &
     &   izero, fld%istack_component, fld%ntot_phys)
      fld%num_phys_viz = fld%num_phys
      fld%ntot_phys_viz = fld%ntot_phys
!
      if(iflag_debug .eq. 0) then
        do ifld = 1, fld%num_phys
          write(*,*) 'fld', ifld, trim(fld%phys_name(ifld)),            &
     &                 fld%istack_component(ifld)
        end do
      end if
!
      end subroutine init_field_name_4_assemble_ucd
!
! ----------------------------------------------------------------------
!
      subroutine assemble_field_data                                    &
     &         (nprocs_org, asbl_comm, new_fld, t_IO, org_fIO)
!
      use t_phys_data
      use t_field_data_IO
      use t_comm_table_4_assemble
!
      use set_field_to_restart
      use field_IO_select
      use share_field_data
!
      integer, intent(in) :: nprocs_org
      type(comm_table_4_assemble), intent(in) :: asbl_comm
!
      type(phys_data), intent(inout) :: new_fld
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: org_fIO(nprocs_org)
!
      integer(kind = kint) :: ist, num, ip
!
!
      call share_time_step_data(t_IO)
      do ip = 1, nprocs_org
        call share_field_IO_names(ip, org_fIO(ip))
        call share_each_field_IO_data(ip, org_fIO(ip))
!
        ist = asbl_comm%istack_recv(ip-1) + 1
        num = asbl_comm%istack_recv(ip) - asbl_comm%istack_recv(ip-1)
        call copy_field_data_4_assemble                                 &
     &     (num, asbl_comm%item_send(ist), asbl_comm%item_recv(ist),    &
     &      org_fIO(ip), new_fld)
!
        call dealloc_merged_field_stack(org_fIO(ip))
        call dealloc_phys_data_IO(org_fIO(ip))
        call dealloc_phys_name_IO(org_fIO(ip))
      end do
!
      end subroutine assemble_field_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_fields_4_assemble_ucd                            &
     &         (nfld_label, ucd_on_label, fld_IO, num_phys)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(in) :: nfld_label
      character(len=kchara), intent(in) :: ucd_on_label(nfld_label)
!
      integer(kind = kint), intent(inout) :: num_phys
!
      integer(kind = kint) :: ifld, jfld
!
!
      num_phys = 0
      do ifld = 1, nfld_label
        do jfld = 1, fld_IO%num_field_IO
          if(ucd_on_label(ifld) .eq. fld_IO%fld_name(jfld)) then
            num_phys = num_phys + 1
            exit
          end if
        end do
      end do
!
      end subroutine count_fields_4_assemble_ucd
!
! ----------------------------------------------------------------------
!
      subroutine set_field_name_4_assemble_ucd                          &
     &         (nfld_label, ucd_on_label, fld_IO, fld)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(in) :: nfld_label
      character(len=kchara), intent(in) :: ucd_on_label(nfld_label)
!
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: ifld, jfld, icou
!
!
      icou = 0
      do ifld = 1, nfld_label
        do jfld = 1, fld_IO%num_field_IO
          if(ucd_on_label(ifld) .eq. fld_IO%fld_name(jfld)) then
            icou = icou + 1
            fld%phys_name(icou) = ucd_on_label(ifld)
            fld%num_component(icou) = fld_IO%istack_comp_IO(jfld)       &
     &                             - fld_IO%istack_comp_IO(jfld-1)
          end if
        end do
      end do
!
      end subroutine set_field_name_4_assemble_ucd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_field_data_4_assemble                             &
     &         (num_item, item_send, item_recv, fld_IO, fld)
!
      integer(kind = kint), intent(in) :: num_item
      integer(kind = kint), intent(in) :: item_send(num_item)
      integer(kind = kint), intent(in) :: item_recv(num_item)
      type(field_IO), intent(in) :: fld_IO
!
      type(phys_data), intent(inout) :: fld
!
!
      integer(kind = kint) :: ifld, ist_org,ist_new, jfld, nd
      integer(kind = kint) :: inum, inod_org, inod_new, ncomp
!
!
      do ifld = 1, fld%num_phys
        do jfld = 1, fld_IO%num_field_IO
          if(fld%phys_name(ifld) .eq. fld_IO%fld_name(jfld)) then
            ist_org = fld_IO%istack_comp_IO(jfld-1)
            ist_new = fld%istack_component(ifld-1)
            ncomp = fld%istack_component(ifld)                          &
     &             - fld%istack_component(ifld-1)
!omp parallel
            do nd = 1, ncomp
!omp do private(inum,inod_new,inod_org)
              do inum = 1, num_item
                inod_new = item_recv(inum)
                inod_org = item_send(inum)
                fld%d_fld(inod_new,ist_new+nd)                          &
     &                   = fld_IO%d_IO(inod_org,ist_org+nd)
              end do
!omp end do
            end do
!omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine copy_field_data_4_assemble
!
! ----------------------------------------------------------------------
!
      end module assemble_nodal_fields
