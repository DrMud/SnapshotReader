module snapshotreader2
  ! read Gadget Format2 Snapshot

  ! STORE VARIABLES
  integer :: nparticles ! number of particles in snapshot
  real*4, allocatable :: allPartCoords(:,:) ! [xpos,ypos,zpos] for each particle (units Mpc/h)
  real*4, allocatable :: allPartMasses(:) ! masses for each particle (units Msol/h)
  integer, allocatable :: allIDlist(:) ! ID's for each particle
  real*8, save :: BoxSize ! Mpc/h
  real*8, save :: OmegaM0,OmegaL0,hubb !dimensionless
  real*8, save :: massArr(6)    ! masses are in 10**10 Msol/h


contains

  subroutine readHeader(snap,flag,z)
    !Read GADGET snapshot header only
    !WARNING: snapshots made using Chris Power's ICs generator are 
    !         different from the standard. These have box sizes and positions
    !         in units of Mpc/h (CP's ICs) rather than kpc/h (standard)
    !WARNING: Masses may also be in different units
    !         Chris Power's ICs generator writes masses in units of 10**10*Msol/h
    !         The standard units are....
    implicit none
    character(len=*),intent(in):: snap
    integer,intent(in) :: flag
    real*4, intent(out) :: z
    real*8 :: dz !double precision version
    real*8 :: dummyD
    integer :: dummyIa(10)
    integer :: narr(6)
    character(len=4) :: blocklabel
    integer :: blocksize
    write(*,*) 'READING HEADER of ',trim(snap)
    write(*,*) '--------------------'
    open(44, FILE=trim(snap),form='unformatted')
    read(44) blocklabel,blocksize
    write(*,*) blocklabel, blocksize
    call flush(6)
    read(44) narr,massArr,dummyD,dz,dummyIa,BoxSize,OmegaM0,OmegaL0,hubb
    write(*,*) narr,massArr,dummyD,dz,dummyIa,BoxSize,OmegaM0,OmegaL0,hubb
    call flush(6)
    close(44)
    select case(flag)
    case(0)
       BoxSize = BoxSize/1000. ! convert to Mpc/h
    end select
    nparticles = sum(narr)
    z = real(dz)
    write(*,*) BoxSize, nparticles, z
    call flush(6)
  end subroutine readHeader



  subroutine readGADGET(snap,flag)
    ! read and store information on every particle in simulation
    !WARNING: snapshots made using Chris Power's ICs generator are 
    !         different from the standard. These have box sizes and positions
    !         in units of Mpc/h (CP's ICs) rather than kpc/h (standard)
    !WARNING: Masses may also be in different units
    !         Chris Power's ICs generator writes masses in units of 10**10*Msol/h
    !         The standard units are....
    implicit none
    integer,intent(in) :: flag
    character(len=*),intent(in)     :: snap
    integer             :: i,j,k
    integer             :: ntype(6), nMasses, ntypei
    real*4, allocatable :: masses(:)
    logical :: fexist
    character(len=4) :: blocklabel
    integer :: blocksize
    !check snapshot exists
    inquire(file=trim(snap) , exist=fexist)
    if(fexist.eqv..false.)then
       write(*,*) 'Cannot find ',trim(snap)
       stop
    end if
    write(*,*) 'READING HEADER (again) of ',trim(snap)
    write(*,*) '--------------------'
1   format(I13,A,I1,T35,A,E12.5,A)
    open(45, FILE=trim(snap),form='unformatted')
    read(45) blocklabel,blocksize
    write(*,*) blocklabel, blocksize
    call flush(6)
    read(45) ntype
    ! check number of particles!
    if(nparticles.ne.sum(ntype)) stop 'Confusion with # total particles'
    ! Calculate number of particle masses listed in the 'masses' block
    nMasses = 0
    do i=0,5 ! particle type
       write(*,1) ntype(i+1),' particles of type ',i,': mass =',&
            &     massArr(i+1)*1e10,' (Msol/h)'
       call flush(6)
       if(massArr(i+1).eq.0)then !if mass varies for this type
          nMasses = nMasses + ntype(i+1)
       end if
    end do
    write(*,*) 'number of particles listed in mass block =',nMasses
    write(*,*)
    call flush(6)
    if(nMasses.gt.0)then
       allocate(masses(nMasses))
    end if
    ! Positions are in Mpc/h (CP's ICs) or kpc/h (standard)
    read(45) blocklabel,blocksize
    write(*,*) blocklabel, blocksize
    call flush(6)
    read(45) (allPartCoords(i,1),allPartCoords(i,2),allPartCoords(i,3), i=1,nparticles)
    select case(flag)
    case(0)
       allPartCoords(:,1:3) = allPartCoords(:,1:3)/1000. ! convert to Mpc/h
    end select
    read(45) blocklabel,blocksize
    write(*,*) blocklabel, blocksize
    call flush(6)
    read(45) ! no need to read in velocities
    read(45) blocklabel,blocksize
    write(*,*) blocklabel, blocksize
    call flush(6)
    read(45) allIDlist
    if(nMasses.gt.0)then
       read(45) blocklabel,blocksize
       write(*,*) blocklabel, blocksize
       call flush(6)
       read(45) (masses(i), i=1,nMasses)
    end if
    write(*,*) 'most massive particle IN BLOCK (Msol/h) = ',maxval(masses)*1d10
    close(45)
    k=0 ! count through each particle in simulation
    j=0 ! count through particle masses listed in 'masses' block
    do i=0,5 ! particle type
       ntypei = ntype(i+1) ! number of particles of that type
       if(ntypei.gt.0)then
          if(massArr(i+1).gt.0.) then ! all particles this mass
             allPartMasses(k+1:k+ntypei) = real(massArr(i+1))
             write(*,*) 'particles of type',i,'have the same mass'
          else ! particles of varying masses
             write(*,*) 'particles of type',i,'have varying mass'
             allPartMasses(k+1:k+ntypei) = masses(j+1:j+ntypei)
             j = j + ntypei
          end if
          k = k + ntypei
       end if
    end do
    !WARNING: masses are in 10**10 Msol/h (standard)
    !convert masses to units Msol/h
    allPartMasses = allPartMasses * 1e10
    write(*,*) 'most massive particle (Msol/h) = ',maxval(allPartMasses)
    if(allocated(masses))then
       deallocate(masses)
    end if
    write(*,*)
    if (.not.(k.eq.nparticles)) stop 'how many particles have been included?'
    if (.not.(j.eq.nmasses)) stop 'how many particles have masses included?'
    write(*,*) 'Finished reading information from GADGET file'
  end subroutine readGADGET



end module snapshotreader2
